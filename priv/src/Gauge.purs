module App.Gauge where
  
import Prelude
import Data.Maybe
import Data.Array
import App.StatsTypes
import Data.Boolean
import Data.Map as Map
import Control.Bind ((=<<))
import Data.Map (Map)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Pux.CSS (FontFaceFormat(..))

import Pux.Html.Events (onClick)
import Pux.Html.Attributes (style)
import Pux.Html as H


data Action = UpdateStats LBStats | ChangeView ViewType

data ViewType = PerNode | PerBroker | PerNodeAndBroker | Aggregate

type State = 
    { viewType:: ViewType
    , stats:: LBStats }

derive instance viewTypeEq :: Eq ViewType

instance viewTypeShow :: Show ViewType where
  show PerNode = "Per Node"
  show PerBroker = "Per Broker"
  show PerNodeAndBroker = "Per Node And Broker"
  show Aggregate = "Aggregate"

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


viewHealthyNodeAs :: ({ node :: Host, brokers:: Array BrokerStats} -> H.Html Action) -> NodeStats -> H.Html Action
viewHealthyNodeAs _ (BadNode node) = viewBadNode node
viewHealthyNodeAs f (NodeStats a) = f a


viewAs :: ViewType -> (Array NodeStats) -> H.Html Action
viewAs PerNode s = 
    H.div [] (viewNodeStats <$> s)

viewAs PerBroker nodes =
    let 
        statsPerBroker = nodes # concatMap _brokers
                           >>> map (\{broker, stats} -> Tuple broker stats)
                           >>> Map.fromFoldableWith  (<>)
                           >>> Map.toAscUnfoldable
    in
    H.div []
    (statsPerBroker # map \(Tuple broker agg) ->
         H.div [] 
            [ H.h1  [] [H.text $ show broker]
            , viewBStats agg]
    )

viewAs PerNodeAndBroker nodes = 
    H.div [] $
    viewNodeAndBrokerStats <$> nodes

viewAs Aggregate nodes = 
    let agg = numConnections nodes in
    H.div [] 
        [ H.h1  [] [H.text "Aggregate Stats"]
        , viewBStats agg]

viewNodeStats :: NodeStats -> H.Html Action
viewNodeStats (NodeStats {node,brokers}) =
    let agg = brokers # map _.stats >>> fold in
    H.div [] 
        [ H.h1  [] [H.text $ show node]
        , viewBStats agg ]

viewNodeStats (BadNode node) =
   viewBadNode node

viewNodeAndBrokerStats :: NodeStats -> H.Html Action
viewNodeAndBrokerStats (NodeStats {node,brokers}) =
    H.div [] $
        brokers # map \brokerStats ->
            H.div []
                    [ H.h1  [] [H.text $ "Node: " <> show node <> " Broker: " <> show brokerStats.broker]
                    , viewBStats brokerStats.stats]

viewNodeAndBrokerStats (BadNode node) =
         viewBadNode node

viewBadNode :: forall a b. (Show b) => b -> H.Html a
viewBadNode node =
     H.div [] 
        [ H.h1  [] [H.text $ show node]
        , H.h2  [style alertStyle] [H.text "Node Down"] ]

viewBStats :: BStats -> H.Html Action
viewBStats stats = 
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
    [Tuple "color" "blue"]

alertStyle :: Array (Tuple String String)
alertStyle = 
    [Tuple "color" "red"]

availableViews :: Array ViewType
availableViews =
   [PerNode, PerBroker, PerNodeAndBroker, Aggregate]

numConnections :: Array NodeStats -> BStats
numConnections = 
    concatMap _brokers
    >>> map _.stats
    >>> fold

areNodeStatsOnline :: NodeStats -> Boolean
areNodeStatsOnline (NodeStats _) = true
areNodeStatsOnline (BadNode _) = false




