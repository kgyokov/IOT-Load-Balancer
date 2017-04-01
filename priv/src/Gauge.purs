module App.Gauge where
  
import Prelude
import Data.Maybe
import Data.Array
import Data.Map as Map
import Data.Map (Map)
import Data.Monoid (class Monoid, mempty)
import Data.Time (Time)
import Data.Tuple (Tuple(..))

import Pux.Html.Events (onClick)
import Pux.Html.Attributes (style)
import Pux.Html as H


data Action = UpdateStats LBStats | ChangeView ViewType

data ViewType = PerNode | PerBroker | PerNodeAndBroker | Aggregate

type LBStats = Array NodeStats

type TimedVal a = { time::Time, value::a }
type State = 
    { viewType:: ViewType
    , stats:: LBStats }

-- /todo: Add Node = String | IPAddress
newtype BStats = BStats Int

type Node = String

newtype Broker = Broker { name :: Node, port :: Int }

type BrokerStats = 
    { broker :: Broker
    , stats :: BStats}

type NodeStats =  
    { node :: Node
    , brokers:: Array BrokerStats}



derive instance viewTypeEq :: Eq ViewType

instance viewTypeShow :: Show ViewType where
  show PerNode = "Per Node"
  show PerBroker = "Per Broker"
  show PerNodeAndBroker = "Per Node And Broker"
  show Aggregate = "Aggregate"

instance bstatsSemigroup :: Semigroup BStats where
    append (BStats b1) (BStats b2 ) = BStats (b1 + b2)
instance bstatsMonoid :: Monoid BStats where
    mempty = BStats 0
instance bstatsShow :: Show BStats where
    show (BStats b) = show b

derive instance brokerEq :: Eq Broker
derive instance brokerOrd :: Ord Broker 

instance brokerShow :: Show Broker where
    show (Broker {name,port}) = show name <> ":" <> show port


init :: State
init = { viewType : PerBroker, stats : [] }

update :: Action -> State -> State
update (UpdateStats us) s = s {stats = us}
update (ChangeView vt) s = s {viewType = vt}

view:: State -> H.Html Action
view {viewType,stats} =
    H.div []
        [ H.h1 [] [numConnectionsView stats]
        , selectView viewType 
        , stats # viewAs viewType ]

numConnectionsView :: (Array NodeStats) -> H.Html Action
numConnectionsView stats = 
    H.text $ stats # numConnections >>> show

viewAs :: ViewType -> (Array NodeStats) -> H.Html Action
viewAs PerNode s = 
    H.div []
    (s # map \{node,brokers} ->
        let agg = brokers # map _.stats >>> fold in
        H.div [] 
            [ H.h1  [] [H.text $ show node]
            , viewStats agg ])

viewAs PerBroker s =
    let 
        mergeBrokerStats {broker, stats} = addOrInsert stats broker
        statsPerBroker = s # concatMap _.brokers 
                           >>> foldr mergeBrokerStats Map.empty
                           >>> Map.toAscUnfoldable
    in
    H.div []
    (statsPerBroker # map \(Tuple broker agg) ->
         H.div [] 
            [ H.h1  [] [H.text $ show broker]
            , viewStats agg]
    )

viewAs PerNodeAndBroker s = 
    H.div [] $
    s # concatMap (\{node,brokers} -> brokers # map \brokerStats -> {node,brokerStats})
    >>> (map \{node,brokerStats} ->
                H.div []
                [ H.h1  [] [H.text $ "Node: " <> show node <> " Broker: " <> show brokerStats.broker]
                    , viewStats brokerStats.stats
        ])

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

selectView :: ViewType -> H.Html Action
selectView viewType = 
    H.ul [] $
     optionView viewType <$> availableViews

optionView :: ViewType -> ViewType -> H.Html Action
optionView sel v =
    H.li
        (if v == sel then [style selectedStyle] else [onClick $ const (ChangeView v)])
        [H.text $ show v]

selectedStyle :: Array (Tuple String String)
selectedStyle = 
    [Tuple "color" "red"]

availableViews :: Array ViewType
availableViews =
   [PerNode, PerBroker, PerNodeAndBroker, Aggregate]

numConnections :: Array NodeStats -> BStats
numConnections = 
    concatMap _.brokers
    >>> map _.stats
    >>> fold

mapWithDefault :: forall a b. (Monoid a) => (a -> b) -> Maybe a -> Maybe b
mapWithDefault f =  emptyIfNothing >>> (<$>) f

addOrInsert :: forall k v. (Monoid v, Ord k) => v -> k -> Map k v -> Map k v
addOrInsert = Map.alter <<< mapWithDefault <<< (<>)

emptyIfNothing :: forall f. Monoid f => Maybe f -> Maybe f
emptyIfNothing Nothing = Just mempty
emptyIfNothing ja = ja


