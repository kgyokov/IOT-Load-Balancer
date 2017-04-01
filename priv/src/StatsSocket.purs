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
import Data.Either
import Data.Argonaut
import Data.Argonaut.Parser --(decodeJson, jsonEmptyObject, (~>), (:=), (.?))
import App.StatsTypes

setupWs :: forall eff. Channel LBStats -> String -> Eff (ws::WEBSOCKET, err::EXCEPTION | eff) Unit
setupWs chan url = do
  Connection ws <- newWebSocket (URL url) []

  ws.onopen $= \_ -> do
    --traceAnyM event
    log "onopen: Connection opened"
    log <<< runURL =<< get ws.url

  ws.onmessage $= \event -> do
    --traceAnyM event
    let received = (getJsonFromEvent event >>= decodeLBStats) :: (Either String LBStats)
    case received of 
      Left msg -> log $ "unknown message received: '" <> msg <> "'"
      Right stats -> 
        --log $ "stats received: '" <> stats <> "'"
        send chan stats
    

  ws.onclose $= \_ -> do
    --traceAnyM event
    log "onclose: Connection closed"

getJsonFromEvent = 
  runMessageEvent >>> runMessage >>> jsonParser


