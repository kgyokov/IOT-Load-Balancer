module Main where

import Prelude
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Document (doctype)
--import Debug.Trace (traceAnyM)

import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)

import Signal ((~>))
import Signal.Channel (channel, subscribe)

import WebSocket(WEBSOCKET)

import App.Layout (Action(..), State, view, update)
import App.Routes (match)
import App.StatsSocket as Socket

type AppEffects = (dom :: DOM)

-- | App configuration
config :: forall eff. State -> Eff (dom :: DOM, ws :: WEBSOCKET | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl
    
  -- | Create a signal for WebSocket stats data
  wsInput <- channel Nop
  statsSig <- Socket.setupWs wsInput "ws://localhost:12000/stats"
  let wsSignal = subscribe wsInput

  let routeSignal = urlSignal ~> \r -> PageView (match r)
  let statsSignal = wsSignal ~> \r -> ReceivedStats r
  -- | Map a signal of URL changes to PageView actions.

  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [routeSignal, wsSignal] }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app


