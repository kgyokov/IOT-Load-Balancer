module App.StatsSocket where

import Prelude
import Signal.Channel
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=), get)
import Data.Either (Either(..))
import Data.Argonaut.Parser (jsonParser)
import WebSocket (Connection(Connection), URL(URL), WEBSOCKET, newWebSocket, runMessage, runMessageEvent, runURL)
import App.StatsTypes (LBStats, decodeLBStats)

setupWs :: forall eff. Channel LBStats -> String -> Eff (ws::WEBSOCKET, err::EXCEPTION | eff) Unit
setupWs chan url = do
  Connection ws <- newWebSocket (URL url) []

  ws.onopen $= \_ -> do
    log "onopen: Connection opened"
    log <<< runURL =<< get ws.url

  ws.onmessage $= \event -> do
    let msg = event # runMessageEvent 
                    >>> runMessage 
    let decoded = (msg # jsonParser 
                   >>= decodeLBStats) :: Either String LBStats
    case decoded of 
      Left error -> log $ "unknown message received: '" <> msg <>" with error: " <> error <> "'"
      Right stats -> send chan stats
    
  ws.onclose $= \_ -> do
    log "onclose: Connection closed"
  


