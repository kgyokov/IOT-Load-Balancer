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
  suite "convertToWsUrl" do
    test "Port+Path+File+Query" do
      url <- liftEff $ convertToWsUrl "http://localhost:12000/static/index.html?q=123"
      Assert.equal url "ws://localhost:12000/ws/stats"
    test "Port+Path" do
      url <- liftEff $ convertToWsUrl "http://localhost:12000/static/"
      Assert.equal url "ws://localhost:12000/ws/stats"
    test "Port+EmptyPath" do     
      url <- liftEff $ convertToWsUrl "http://test.org:80/"
      Assert.equal url "ws://test.org:80/ws/stats"
    test "Port" do
      url <- liftEff $ convertToWsUrl "http://test.org:80"
      Assert.equal url "ws://test.org:80/ws/stats"
    test "Relative" do
      e <- liftEff' $ convertToWsUrl "/static/index.html"
      either (const success) (\r -> failure $ "Converted invalid URL to " <> r) e

  

