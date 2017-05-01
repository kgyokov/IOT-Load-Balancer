module App.Layout where

import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import App.StatsTypes
import App.StatsSocket as Socket
import App.Gauge as Gauge
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = PageView Route
  | Stats Gauge.Action
  | Received Socket.Action

type State =
  { route :: Route
  , stats :: Gauge.State }

init :: State
init =
  { route: NotFound
  , stats: Gauge.init }


update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Stats action) state = state { stats = Gauge.update action state.stats }
update (Received (Socket.NewStats action)) state = state { stats = Gauge.update (Gauge.UpdateStats action) state.stats }
update (Received _) state = state -- TODO: Present socket retires (visual status???)


view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "IOT Load Balancer UI" ]
    , p [] [ text "Select a view style for the Broker/Load Balancer stats." ]
    , map Stats $ Gauge.view state.stats
      -- case state.route of
      --   Home -> map Stats $ Gauge.view state.stats
      --   NotFound -> NotFound.view state
    ]
