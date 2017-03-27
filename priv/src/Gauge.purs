module Gauge where
  
import Prelude
import Data.List
import Data.Maybe
import Data.Functor
import Control.Monad.Eff.Exception
import Pux.Html as H
import Pux.Html.Attributes as A
import Pux.Html.Events as E
import Control.Bind ((=<<))
import Data.Foldable (sum)
import Data.Eq (Eq)

import React.DOM.Props (onChange)

data ViewType = PerNode | PerBroker | PerNodeAndBroker | Aggregate deriving Eq

data Action = UpdateStats (List NodeStats) | ChangeView ViewType

type TimedVal a = { time::Time, value::a }
type State = 
    { viewType:: ViewType
    , stats:: List NodeStats }

-- /todo: Add Node = String | IPAddress
type Node = String
type Broker = 
    { name :: Node
    , port :: Int }
type BStats = Int
type BrokerStats = 
    { broker ::Broker
    , connections :: BStats}

type NodeStats =  
    { node ::  Node
    , timedSeries:: List (TimedVal (List BrokerStats))}

instance viewTypeShow :: Show ViewType where
  show PerNode = "PerNode"
  show PerBroker = "PerBroker"
  show PerNodeAndBroker = "PerNodeAndBroker"
  show Aggregate = "Aggregate"

numConnections :: List NodeStats -> Int
numConnections = 
    concatMap latestStatsPerBroker
    >>> map _.connections
    >>> sum

-- numConnections :: List BrokerStats -> Int 
-- numConnections = 
--     map _.connections >>> sum

latestStatsPerBroker :: NodeStats -> List BrokerStats
latestStatsPerBroker = 
     _.timedSeries 
        >>> head 
        >>> map _.value
        >>> emptyIfNothing


-- latestStatsPerBroker :: List(NodeStats) -> List BrokerStats
-- latestStatsPerBroker = 
--     map latestStatsPerBroker
--     >>> group _.broker
--     >>> {broker : b, connections : }

emptyIfNothing :: forall a. Maybe (List a) -> List a
emptyIfNothing (Just x) = x
emptyIfNothing Nothing = Nil

init :: State
init = { viewType : PerBroker, stats : Nil }

update :: Action -> State -> State
update (UpdateStats us) s = s {stats = us}
update (ChangeView vt) s = s {viewType = vt}

view:: State -> H.Html Action
view {viewType,stats} =
    H.div
    []
    [ H.h1 [] [numConnectionsView stats]
    , select viewType 
    , stats # viewAs viewType
    ]

numConnectionsView :: (List NodeStats) -> H.Html Action
numConnectionsView stats = 
    H.text $ stats # show <<< numConnections

viewAs :: ViewType ->  (List NodeStats) -> H.Html Action
viewAs PerNode s = H.div [] []
viewAs PerBroker s = H.div [] []
viewAs PerNodeAndBroker s = H.div [] []
viewAs Aggregate s = H.div [] []

select :: ViewType -> H.Html Action
select viewType = 
    H.select
     [E.onSelect changeView]
     (optionView <$> availableViews)

optionView :: ViewType -> ViewType -> H.Html Action
optionView v sel =
    let strVal = viewTypeToStr v in
    H.option [A.value strVal, A.selected (v == sel) ] [H.text strVal]

changeView :: E.SelectionEvent -> Action
changeView se = 
    ChangeView (toViewType se.target.value)

availableViews :: Array ViewType
availableViews =
   [PerNode, PerBroker, PerNodeAndBroker, Aggregate]

viewTypeToStr:: ViewType -> String
viewTypeToStr = show

toViewType :: String -> ViewType
toViewType "PerNode" = PerNode
toViewType "PerBroker" = PerBroker
toViewType "PerNodeAndBroker" = PerNodeAndBroker
toViewType "Aggregate" = Aggregate
toViewType _ = Aggregate
