module Test.Main where

import Prelude
import Main

import Test.Unit (suite, test, timeout, success, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Data.Either
import Control.Monad.Eff (Eff)
import Control.Monad.Aff
import Control.Monad.Eff.Class --liftEff
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, catchException, throwException)

--main :: forall e. Eff (assert::ASSERT, err::EXCEPTION, console :: CONSOLE | e) Unit
main = runTest do
  log "write some tests"

  

