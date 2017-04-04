module App.StatsTypes where

import Prelude
import Data.Maybe
import DOM.Node.Document (doctype)
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, encodeJson, fromArray, decodeJson, jsonEmptyObject, (~>), (:=), (.?))
import Data.Monoid (class Monoid)
import Data.Time (Time)
import Data.Traversable (traverse)


type LBStats = Array NodeStats
data NodeStats = NodeStats { node :: Host, brokers:: Array BrokerStats} 
             --  | BadNode Host
type BrokerStats = { broker :: Broker, stats :: BStats}
newtype Broker = Broker { host :: Host, port :: Int }
newtype BStats = BStats Int
type Host = String

-- /todo: Add Host = String | IPAddress

type TimedVal a = { time::Time, value::a }

-------------------------------------------------------------------
--- ACCESSORS
-------------------------------------------------------------------

_host :: Broker -> Host
_host (Broker {host}) = host

_port :: Broker -> Int
_port (Broker {port}) = port

_brokers :: NodeStats -> Array BrokerStats
_brokers (NodeStats {brokers}) = brokers

-------------------------------------------------------------------
--- Type Class instances
-------------------------------------------------------------------

instance bstatsSemigroup :: Semigroup BStats where
    append (BStats b1) (BStats b2 ) = BStats (b1 + b2)
instance bstatsMonoid :: Monoid BStats where
    mempty = BStats 0
instance bstatsShow :: Show BStats where
    show (BStats b) = show b

derive instance brokerEq :: Eq Broker
derive instance brokerOrd :: Ord Broker 

instance brokerShow :: Show Broker where
    show (Broker {host,port}) = show host <> ":" <> show port


-------------------------------------------------------------------
-- JSON PARSING
-------------------------------------------------------------------
instance bstatsDecodeJson :: DecodeJson BStats where
    decodeJson json = do
        val <- decodeJson json
        pure $ BStats val

instance brokerDecodeJson :: DecodeJson Broker where
    decodeJson json = do
        obj <- decodeJson json
        host <- obj.? "host"
        port <- obj.? "port"
        pure $ Broker {host,port}

-- instance brokerStatsDecodeJson :: DecodeJson BrokerStats where
--      decodeJson json = do
--         obj <- decodeJson json
--         broker <- obj.? "broker"
--         stats <- obj.? "stats"
--         pure $ BrokerStats {broker,stats}

instance nodeStatsDecodeJson :: DecodeJson NodeStats where
     decodeJson json = do
        obj <- decodeJson json
        node <- obj.? "node"
        brokersJs <- obj.? "brokers"
        brokers <- traverse decodeBrokerStats brokersJs
        pure $ (NodeStats {node,brokers})


--- TODO: Maybe convert these to newtypes (as opposed to using them as simple records)
--- and then simply implement instances of DecodeJson

-- decodeNodeStats json = do
--     obj <- decodeJson json
--     node :: Host <- obj.? "node"
--     brokersJs :: Array Json <- obj.? "brokers" 
--     brokers <- traverse decodeBrokerStats brokersJs
--     pure $ {node, brokers}

decodeBrokerStats json  = do
    obj <- decodeJson json
    broker :: Broker <- obj.? "broker"
    stats :: BStats <- obj.? "stats"
    pure $ {broker, stats}

decodeLBStats json = do
    arr :: Array Json <- decodeJson json
    traverse decodeJson arr

