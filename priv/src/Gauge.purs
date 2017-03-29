module Gauge where
  
import Prelude
import Control.Bind ((=<<))
import Control.Monad.Eff.Exception
import Data.Maybe
import Data.Monoid
import Data.Functor
import Data.List as L
import Data.Map as Map
import Data.Eq (Eq)
import Data.Foldable (sum)
import Data.List (concatMap)
import Data.Map (Map, alter, fromFoldable, update)

import Pux.Html.Events (onClick)
import Pux.Html as H
import Pux.CSS as CSS
import Pux.Html.Attributes as A
import Pux.Html.Events as E

import React.DOM.Props (onChange)

data ViewType = PerNode | PerBroker | PerNodeAndBroker | Aggregate deriving Eq

data Action = UpdateStats (List NodeStats) | ChangeView ViewType

type TimedVal a = { time::Time, value::a }
type State = 
    { viewType:: ViewType
    , stats:: List NodeStats }

-- /todo: Add Node = String | IPAddress
type BStats = Int
type Node = String

type Broker = 
    { name :: Node
    , port :: Int }
type BrokerStats = 
    { broker ::Broker
    , connections :: BStats}

type NodeStats =  
    { node ::  Node
    , brokers:: List BrokerStats}

init :: State
init = { viewType : PerBroker, stats : Nil }

update :: Action -> State -> State
update (UpdateStats us) s = s {stats = us}
update (ChangeView vt) s = s {viewType = vt}

view:: State -> H.Html Action
view {viewType,stats} =
    H.div []
        [ H.h1 [] [numConnectionsView stats]
        , select viewType 
        , stats # viewAs viewType ]

numConnectionsView :: (List NodeStats) -> H.Html Action
numConnectionsView stats = 
    H.text $ stats # show <<< numConnections

viewAs :: ViewType -> (List NodeStats) -> H.Html Action
viewAs PerNode s = 
    H.div [] 
    s # map \{node,brokers} ->
        let agg = brokers # map _.connections # sum in
        H.div [] 
            [ H.h1  [] [text $ show node]
            , viewStats agg ]

viewAs PerBroker s =
    let mergeBrokerStats {broker, connections} = addOrInsert broker connections
        statsPerBroker = s # concatMap _.brokers 
                            >>> foldl mergeBrokerStats Map.empty
                            >>> Map.toAscUnfoldable
    in
    H.div []
    statsPerBroker
    # map \(Tuple broker agg) ->
         H.div [] 
            [ H.h1  [] [text $ show broker]
            , viewStats agg]

viewAs PerNodeAndBroker s = 
    H.div []
    s # concatMap \{node,brokers} -> brokers # map {node,brokerStats}
    >>> map \{node,brokerStats} ->
        [ H.h1  [] [text $ "Node: " <> node <> " Broker: " <> show brokerStats.broker]
            , viewStats brokerStats.connections]

viewAs Aggregate s = 
    let agg = s # numConnections in
    H.div [] 
        [ H.h1  [] [text "Aggregate Stats"]
        , viewStats agg]


viewStats :: BStats -> H.Html Action
viewStats stats = 
    H.div [] 
        [ H.h2 [] [text "Connections:"]
        , H.p  [] [text $ show stats] ]

select :: ViewType -> H.Html Action
select viewType = 
    H.ul []
     (optionView viewType <$> availableViews)

optionView :: ViewType -> ViewType -> H.Html Action
optionView sel v =
    H.li 
        if v == sel 
        then [CSS.style ""]
        else [E.onClick $ const (ChangeView v)]
        [H.text $ show v]

availableViews :: Array ViewType
availableViews =
   [PerNode, PerBroker, PerNodeAndBroker, Aggregate]

instance viewTypeShow :: Show ViewType where
  show PerNode = "Per Node"
  show PerBroker = "Per Broker"
  show PerNodeAndBroker = "Per Node And Broker"
  show Aggregate = "Aggregate"

numConnections :: List NodeStats -> Int
numConnections = 
    concatMap _.brokers
    >>> map _.connections
    >>> sum

upsert :: forall k v. Monoid v => (v -> v) -> k -> Map k v -> Map k v
upsert f = Map.alter (emptyIfNothing >>> f >>> Just)

addOrInsert :: forall k v. Monoid v => v -> k -> Map k v -> Map k v
addOrInsert v = upsert $ (<#>) v

emptyIfNothing :: forall f. Monoid f => Maybe f -> f
emptyIfNothing (Just fa) = fa
emptyIfNothing Nothing = mempty

