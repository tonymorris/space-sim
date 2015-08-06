module Data.Space.Control where

import Prelude

data Control =
  Control
    String -- secret key
    MainEngine
    Rotation
  deriving (Eq, Show)

data MainEngine = 
  MainEngineOn
  | MainEngineOff
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