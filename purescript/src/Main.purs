module Main where

import Prelude
import Data.Either
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION,throwException,error)
import Data.URI.Types (HierarchicalPart(..))

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (pathname)
import DOM.HTML.Window(location)
import Data.URI as U
import Data.Path.Pathy((</>),dir,file,rootDir)
--import Debug.Trace (traceAnyM)

import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)

import Signal ((~>),Signal)
import Signal.Channel (channel, subscribe, CHANNEL)

import WebSocket(WEBSOCKET)

import App.Layout (Action(..), State, view, update, init)
import App.Routes (match)
import App.StatsSocket as Socket

type AppEffects = (dom :: DOM, ws :: WEBSOCKET)

-- | App configuration
config :: forall eff. State -> Eff (dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, ws :: WEBSOCKET | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  routeSignal <- configRoute
  -- | Create a signal for WebSocket stats data
  socketSignal <- configSocket
  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [routeSignal, socketSignal] }

configSocket :: forall eff. Eff (dom :: DOM, err :: EXCEPTION, channel :: CHANNEL, ws :: WEBSOCKET | eff) (Signal Action)
configSocket = do 
  wsUrl <- getWsUrl
  wsInput <- channel Socket.Starting
  statsSig <- Socket.setupWs wsInput wsUrl
  let wsSignal = subscribe wsInput
  let socketSignal = wsSignal ~> Received
  pure socketSignal

configRoute :: forall eff. Eff (dom :: DOM | eff) (Signal Action)
configRoute = do
  urlSignal <- sampleUrl
  -- | Map a signal of URL changes to PageView actions.
  pure $ urlSignal ~> match >>> PageView

getWsUrl :: forall eff. Eff (dom :: DOM, err :: EXCEPTION | eff) String
getWsUrl = getInitialPath >>= convertToWsUrl

convertToWsUrl :: forall eff. String -> Eff (err :: EXCEPTION | eff) String
convertToWsUrl url = 
  case U.runParseURI url of
    Right 
      (U.URI _ 
        h@(U.HierarchicalPart 
            auth@(Just authority) 
            (Just path) 
          )
       _  _
      ) ->
      let wsScheme = Just (U.URIScheme "ws")
          wsPath = (Just (Right (rootDir </> dir "ws" </> file "stats")))
          wsH = U.HierarchicalPart auth wsPath
      in
      pure $ U.printURI $ U.URI wsScheme wsH Nothing Nothing
    _ -> throwException $ error "Could not get the URL for the stats websocket socket"

getInitialPath :: forall eff. Eff (dom :: DOM | eff) String
getInitialPath =  window >>= location >>= pathname

-- | Entry point for the browser.
main :: Eff (CoreEffects AppEffects) (App State Action)
main = do
    app <- start =<< config init
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


