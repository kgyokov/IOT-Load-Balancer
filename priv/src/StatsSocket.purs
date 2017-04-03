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
import Data.Argonaut.Parser
import App.StatsTypes

setupWs :: forall eff. Channel LBStats -> String -> Eff (ws::WEBSOCKET, err::EXCEPTION | eff) Unit
setupWs chan url = do
  Connection ws <- newWebSocket (URL url) []

  ws.onopen $= \_ -> do
    log "onopen: Connection opened"
    log <<< runURL =<< get ws.url

  ws.onmessage $= \event -> do
    case getLBStatsFromEvent event of 
      Left msg -> log $ "unknown message received: '" <> msg <> "'"
      Right stats -> send chan stats
    

  ws.onclose $= \_ -> do
    log "onclose: Connection closed"

--getLBStatsFromEvent :: MessageEvent -> Either String LBStats
getLBStatsFromEvent event = 
  (event # runMessageEvent 
            >>> runMessage 
            >>> jsonParser
            >>= decodeLBStats) :: Either String LBStats
  


