module App.StatsSocket where

import Prelude
import Signal.Channel
import App.StatsTypes (LBStats, decodeLBStats)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=), get)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import WebSocket (Connection(Connection), URL(URL), WEBSOCKET, newWebSocket, runMessage, runMessageEvent, runURL)


data Action = 
      NewStats LBStats
    | ChannelError
    | ChannelClosed
    | Starting

setupWs :: forall eff. Channel Action -> String -> Eff (ws::WEBSOCKET, err::EXCEPTION | eff) Unit
setupWs chan url = do
  Connection ws <- newWebSocket (URL url) []

  ws.onopen $= \_ -> do
    log "onopen: Connection opened"
    log <<< runURL =<< get ws.url

  ws.onmessage $= \event ->
    let msg = event # runMessageEvent 
                    >>> runMessage 
        decoded = (msg # jsonParser 
                   >>= decodeLBStats) :: Either String LBStats
    in
    case decoded of 
      Left error -> do
        log $ "unknown message received: '" <> msg <>" with error: " <> error <> "'"
        send chan ChannelError
      Right stats -> send chan (NewStats stats)
      
  ws.onerror $= \_ -> do
    log "onerror: Connection error"
    send chan ChannelError

  ws.onclose $= \_ -> do
    log "onclose: Connection closed"
    send chan ChannelClosed

  


