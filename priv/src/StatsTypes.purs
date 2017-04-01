module App.StatsTypes where

import Prelude
import Data.Maybe
import Data.Monoid (class Monoid, mempty)
import Data.Time (Time)

type LBStats = Array NodeStats

type TimedVal a = { time::Time, value::a }

-- /todo: Add Node = String | IPAddress
newtype BStats = BStats Int

type Node = String

newtype Broker = Broker { name :: Node, port :: Int }

type BrokerStats = 
    { broker :: Broker
    , stats :: BStats}

type NodeStats =  
    { node :: Node
    , brokers:: Array BrokerStats}

instance bstatsSemigroup :: Semigroup BStats where
    append (BStats b1) (BStats b2 ) = BStats (b1 + b2)
instance bstatsMonoid :: Monoid BStats where
    mempty = BStats 0
instance bstatsShow :: Show BStats where
    show (BStats b) = show b

derive instance brokerEq :: Eq Broker
derive instance brokerOrd :: Ord Broker 

instance brokerShow :: Show Broker where
    show (Broker {name,port}) = show name <> ":" <> show port