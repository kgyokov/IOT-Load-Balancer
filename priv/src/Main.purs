module Main where

import Prelude
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Document (doctype)
--import Debug.Trace (traceAnyM)

import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)

import Signal ((~>))
import Signal.Channel (channel, subscribe, CHANNEL)

import WebSocket(WEBSOCKET)

import App.Layout (Action(..), State, view, update)
import App.Routes (match)
import App.StatsSocket as Socket

type AppEffects = (dom :: DOM, ws :: WEBSOCKET )

-- | App configuration
config :: forall eff. State -> Eff (dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, ws :: WEBSOCKET | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl
    
  -- | Create a signal for WebSocket stats data
  wsInput <- channel []
  statsSig <- Socket.setupWs wsInput "ws://localhost:12000/stats"
  let wsSignal = subscribe wsInput

  let routeSignal = urlSignal ~> match >>> PageView
  let statsSignal = wsSignal ~> ReceivedStats
  -- | Map a signal of URL changes to PageView actions.

  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [routeSignal, statsSignal] }

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


