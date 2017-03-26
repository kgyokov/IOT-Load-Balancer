module Gauge where
  
import Prelude
import Pux.Html as H
import Control.Bind ((=<<))
import Control.Category (id)
import Data.Field (div)
import Data.Foldable (sum)
import Data.List
import Data.StrMap (empty)

data Action = UpdateStats (List NodeStats)
type State = (List NodeStats)

-- /todo: Add Node = String | IPAddress
type Node = String
type Broker = 
    { name :: Node
    , port :: Int }
type BStats = Int
type BrokerStats = 
    { broker ::Broker
    , stats :: BStats}

type NodeStats =  
    { node ::  Node
    , brokers :: List BrokerStats}

numConnections :: List NodeStats -> Int
numConnections = 
    concatMap _.brokers
    >>> map _.stats
    >>> sum

init :: State
init = Nil

update :: Action -> State -> State
update (UpdateStats us) s = us

view:: State -> H.Html Action
view state =
    H.div
    []
    [H.h1 [] [H.text "test" ]]

