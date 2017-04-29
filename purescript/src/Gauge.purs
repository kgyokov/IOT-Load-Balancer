module App.Gauge where
  
import Prelude
import Data.Maybe
import Data.Array
import Data.Boolean
import Data.Map as Map
import CSS (border, marginLeft)
import CSS.Geometry (margin, marginRight)
import Control.Bind ((=<<))
import DOM.Node.Document (doctype)
import Data.Semigroup ((<>))
import Pux.CSS (width)
import Pux.Html.Attributes (z)

import Data.Map (Map)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

import CSS.Display
import CSS.Color
import CSS.Background
import CSS.Border
import CSS.Size
import CSS.Geometry
import CSS (borderRadius, padding)
--import Pux.CSS (class Background)
import Pux.Html.Attributes (style)
import Pux.Html.Events (onClick)
import Pux.CSS as PSS
import Pux.Html as H
import App.StatsTypes


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
    H.div [ centerStyle ]
        [ H.h2 [] [ numConnectionsView stats ]
        , selectView viewType 
        , H.div [ statsDisplayStyle ]
                [ stats # viewAs viewType ]
        ]

numConnectionsView :: (Array NodeStats) -> H.Html Action
numConnectionsView stats = 
    H.text $ "Total Connections: " <> (show $ stats # numConnections)


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
            [ H.h2  [] [H.text $ show broker]
            , viewBStats agg ]
    )

viewAs PerNodeAndBroker nodes = 
    H.div [] $
    viewNodeAndBrokerStats <$> nodes

viewAs Aggregate nodes = 
    let agg = numConnections nodes in
    H.div [] 
        [ 
            H.h2  [] [H.text "Aggregate Stats"]
          , viewBStats agg
        ]

viewNodeStats :: NodeStats -> H.Html Action
viewNodeStats (NodeStats {node,brokers}) =
    let agg = brokers # map _.stats >>> fold in
    H.div [] 
        [ 
            H.h2  [] [H.text $ show node]
          , viewBStats agg 
        ]

viewNodeStats (BadNode node) =
   viewBadNode node

viewNodeAndBrokerStats :: NodeStats -> H.Html Action
viewNodeAndBrokerStats (NodeStats {node,brokers}) =
    H.div [] $
        brokers # map \brokerStats ->
            H.div []
                [ H.h2  [] 
                        [ H.text $ 
                          "Node: " <> show node <>
                          " Broker: " <> show brokerStats.broker ]
                , viewBStats brokerStats.stats
                ]

viewNodeAndBrokerStats (BadNode node) =
         viewBadNode node

viewBadNode :: forall a b. (Show b) => b -> H.Html a
viewBadNode node =
     H.div [] 
        [ H.h2  [] [H.text $ show node]
        , H.h3  [style alertStyle] [H.text "Node Down"] 
        ]

viewBStats :: BStats -> H.Html Action
viewBStats stats = 
    H.div [] 
        [ H.h3 [] [H.text "Connections:"]
          , H.p  [] [H.text $ show stats] 
        ]


selectView :: ViewType -> H.Html Action
selectView viewType = 
    H.div [ PSS.style do 
                    marginLeft (40.0 # px)
                    marginBottom (0.0 # px) ] $
     optionView viewType <$> availableViews

optionView :: ViewType -> ViewType -> H.Html Action
optionView sel v =
    H.div 
         (if v == sel 
            then selectedOptionStyle
            else  [ onClick $ const (ChangeView v) ] 
                  <> nonSelectedOptionStyle
         )
        [H.text $ show v]

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


----------------------------------------------------------------------------
--- STYLES
--- TODO: Move to separate module
----------------------------------------------------------------------------

nonSelectedOptionStyle :: forall a. Array (H.Attribute a)
nonSelectedOptionStyle = 
    [ z "99"
    , PSS.style do 
        optionViewStyle 
        background  (rgb 20 20 250)
    ]

selectedOptionStyle :: forall a. Array (H.Attribute a)
selectedOptionStyle = 
    [ z "1"
    , PSS.style do 
        optionViewStyle
        marginLeft (-10.0 # px)
        marginRight (-10.0 # px)
        background  (rgb 150 150 200 )
    ]

--selectViewStyle :: forall a. H.Attribute a
optionViewStyle :: PSS.StyleM Unit
optionViewStyle  = do
    float floatLeft
    padding (10.0 # px) (25.0 # px) (5.0 # px) (25.0 # px)
    border solid (1.0 # px) (rgb 0xff 0xff 0xff)
    borderRadius (15.0 # px) (15.0 # px) (0.0 # px) (0.0 # px)
    PSS.color (rgb 250 250 250)
 
centerStyle :: forall a. H.Attribute a
centerStyle =
          PSS.style do 
            width (90.0 # pct)
            margin (40.0 # px) (40.0 # px) (40.0 # px) (40.0 # px)

statsDisplayStyle :: forall a. H.Attribute a
statsDisplayStyle = 
    PSS.style do
        clear clearBoth
        minHeight (400.0 # px)
        border solid (2.0 # px) (rgb 0x00 0x00 0xff)
        margin (0.0 # px) (10.0 # px) (10.0 # px) (10.0 # px)
        marginTop (0.0 # px)
        padding (10.0 # px)  (10.0 # px) (10.0 # px) (10.0 # px)
