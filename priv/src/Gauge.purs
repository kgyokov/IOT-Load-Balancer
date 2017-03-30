module Gauge where
  
import Prelude
import Data.Maybe
import Data.Monoid (class Monoid, mempty)
import Data.Array
import Data.Tuple (Tuple(..))
import Data.Time (Time)
import Data.Map as Map
import Data.Map (Map)

import Pux.Html.Events (onClick)
import Pux.Html as H
--import Pux.CSS as CSS
import Pux.Html.Events as E

data ViewType = PerNode | PerBroker | PerNodeAndBroker | Aggregate

derive instance viewTypeEq :: Eq ViewType

data Action = UpdateStats (Array NodeStats) | ChangeView ViewType

type TimedVal a = { time::Time, value::a }
type State = 
    { viewType:: ViewType
    , stats:: Array NodeStats }

-- /todo: Add Node = String | IPAddress
newtype BStats = BStats Int
instance bstatsSemigroup :: Semigroup BStats where
    append (BStats b1) (BStats b2 ) = BStats (b1 + b2)
instance bstatsMonoid :: Monoid BStats where
    mempty = BStats 0
instance bstatsShow :: Show BStats where
    show (BStats b) = show b

type Node = String

newtype Broker = Broker { name :: Node, port :: Int }

derive instance brokerEq :: Eq Broker
derive instance brokerOrd :: Ord Broker 

instance brokerShow :: Show Broker where
    show (Broker {name,port}) = show name <> ":" <> show port
-- instance brokerOrd :: Ord Broker where
--     compare (Broker {name:n1, port:p1}) (Broker {name:n2, port:p2}) = (compare n1 n2) <> (compare p1 p2)

type BrokerStats = 
    { broker ::Broker
    , connections :: BStats}

type NodeStats =  
    { node ::  Node
    , brokers:: Array BrokerStats}

init :: State
init = { viewType : PerBroker, stats : [] }

update :: Action -> State -> State
update (UpdateStats us) s = s {stats = us}
update (ChangeView vt) s = s {viewType = vt}

view:: State -> H.Html Action
view {viewType,stats} =
    H.div []
        [ H.h1 [] [numConnectionsView stats]
        , select viewType 
        , stats # viewAs viewType ]

numConnectionsView :: (Array NodeStats) -> H.Html Action
numConnectionsView stats = 
    H.text $ stats # numConnections >>> show

viewAs :: ViewType -> (Array NodeStats) -> H.Html Action
viewAs PerNode s = 
    H.div []
    (s # map \{node,brokers} ->
        let agg = brokers # map _.connections >>> fold in
        H.div [] 
            [ H.h1  [] [H.text $ show node]
            , viewStats agg ])

viewAs PerBroker s =
    let 
        mergeBrokerStats m {broker, connections} = addOrInsert connections broker m
        statsPerBroker = s # concatMap _.brokers 
                           >>> foldl mergeBrokerStats Map.empty
                           >>> Map.toAscUnfoldable
    in
    H.div []
    (statsPerBroker # map \(Tuple broker agg) ->
         H.div [] 
            [ H.h1  [] [H.text $ show broker]
            , viewStats agg])

viewAs PerNodeAndBroker s = 
    H.div [] $
    s # concatMap (\{node,brokers} -> brokers # map \brokerStats -> {node,brokerStats})
    >>> (map \{node,brokerStats} ->
        H.div []
        [ H.h1  [] [H.text $ "Node: " <> node <> " Broker: " <> show brokerStats.broker]
            , viewStats brokerStats.connections])

viewAs Aggregate s = 
    let agg = numConnections s in
    H.div [] 
        [ H.h1  [] [H.text "Aggregate Stats"]
        , viewStats agg]


viewStats :: BStats -> H.Html Action
viewStats stats = 
    H.div [] 
        [ H.h2 [] [H.text "Connections:"]
        , H.p  [] [H.text $ show stats] ]

select :: ViewType -> H.Html Action
select viewType = 
    H.ul []
     (optionView viewType <$> availableViews)

optionView :: ViewType -> ViewType -> H.Html Action
optionView sel v =
    H.li 
        (if v == sel then [] else [E.onClick $ const (ChangeView v)])
        [H.text $ show v]

availableViews :: Array ViewType
availableViews =
   [PerNode, PerBroker, PerNodeAndBroker, Aggregate]

instance viewTypeShow :: Show ViewType where
  show PerNode = "Per Node"
  show PerBroker = "Per Broker"
  show PerNodeAndBroker = "Per Node And Broker"
  show Aggregate = "Aggregate"

numConnections :: Array NodeStats -> BStats
numConnections = 
    concatMap _.brokers
    >>> map _.connections
    >>> fold

upsert :: forall k v. (Monoid v, Ord k) => (v -> v) -> k -> Map k v -> Map k v
upsert f = Map.alter (emptyIfNothing >>> f >>> Just)

updateJust f = emptyIfNothing >>> f >>> Just

addOrInsert :: forall k v. (Monoid v, Ord k) => v -> k -> Map k v -> Map k v
addOrInsert v = upsert $ (<>) v

emptyIfNothing :: forall f. Monoid f => Maybe f -> f
emptyIfNothing (Just fa) = fa
emptyIfNothing Nothing = mempty

