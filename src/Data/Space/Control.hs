module Data.Space.Control where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack)

data Control =
  Control
    String -- secret key
    MainEngine
    Rotation
  deriving (Eq, Show)

data MainEngine = 
  MainEngineOff
  | MainEngineOn
  deriving (Eq, Show)

data Rotation =
  Clock
  | None
  | AntiClock
  deriving (Eq, Show)

class ToProtocol a where
  toProtocol ::
    a
    -> String

instance ToProtocol Rotation where
  toProtocol Clock =
    "-1"
  toProtocol None =
    "0"
  toProtocol AntiClock =
    "1"

instance ToProtocol MainEngine where
  toProtocol MainEngineOn =
    "1"
  toProtocol MainEngineOff =
    "0"

instance ToProtocol Control where
  toProtocol (Control k e r) =
    concat [k, ",", toProtocol e, ",", toProtocol r]

sendP ::
  (ToProtocol a, Sender t) =>
  Socket z t
  -> [Flag]
  -> a
  -> ZMQ z ()
sendP s x =
  send s x . pack . toProtocol
