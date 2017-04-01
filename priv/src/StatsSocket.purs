module App.StatsSocket where

import Prelude
import Signal
import Signal.Channel
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=), get)

import WebSocket (Connection(..), Message(..), URL(..), runMessageEvent, runMessage, runURL, newWebSocket, WEBSOCKET)

setupWs :: forall eff. Channel String -> String -> Eff (ws::WEBSOCKET, err::EXCEPTION | eff) Unit
setupWs chan url = do
  Connection ws <- newWebSocket (URL url) []

  ws.onopen $= \_ -> do
    --traceAnyM event
    log "onopen: Connection opened"
    log <<< runURL =<< get ws.url

  ws.onmessage $= \event -> do
    --traceAnyM event
    let received = runMessage (runMessageEvent event)
    send chan received
    log $ "onmessage: Received '" <> received <> "'"

  ws.onclose $= \_ -> do
    --traceAnyM event
    log "onclose: Connection closed"

