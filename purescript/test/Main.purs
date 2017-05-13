module Test.Main where

import Prelude
import Main
import Test.Assert
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, catchException, throwException)

main :: forall e. Eff (assert::ASSERT, err::EXCEPTION, console :: CONSOLE | e) Unit
main = do
  convertToWsUrlTests


convertToWsUrlTests :: forall e. Eff (assert::ASSERT, console :: CONSOLE, err::EXCEPTION | e) Unit
convertToWsUrlTests = do 
  url1 <- convertToWsUrl "http://localhost:12000/static/index.html?q=123"
  assert (url1 == "ws://localhost:12000/ws/stats")
  url2 <- convertToWsUrl "http://test.org:80/"
  assert (url2 == "ws://test.org:80/ws/stats")
  url3 <- convertToWsUrl "http://test.org:80"
  assert (url3 == "ws://test.org:80/ws/stats")
  --(catchException ((\_ -> log "Exception caught as expected"):: Error -> Eff (console :: CONSOLE | e) Unit) ((const unit) <$> (convertToWsUrl "./test.org:80")))
  

