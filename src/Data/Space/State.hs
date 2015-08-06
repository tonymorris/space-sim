{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Space.State where

import Prelude

import Data.Aeson
import Control.Monad (mzero)
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as L

data Ship =
  Ship {
    id :: String
  , x :: Float
  , y :: Float
  , vx :: Float
  , vy :: Float
  , theta :: Float
  , omega :: Float
  , tl :: Int
  , tr :: Int
  } deriving (Eq, Show)

instance FromJSON Ship where
    parseJSON (Object v) = Ship
        <$> v .: "id"
        <*> v .: "x"
        <*> v .: "y"
        <*> v .: "vx"
        <*> v .: "vy"
        <*> v .: "theta"
        <*> v .: "omega"
        <*> v .: "Tl" -- grr
        <*> v .: "Tr"
    parseJSON _ = mzero

data GameState
  = FinishedState
  | RunningState [Ship]
  deriving (Eq, Show)

instance FromJSON GameState where
    parseJSON (Object v) = do
        state <- v .: "state"
        case state of
            (String "running") -> do
                players <- v .: "data"
                return (RunningState players)
            _ -> return FinishedState
    parseJSON _ = mzero

exampleRunningState, exampleFinishedState, exampleShip :: L.ByteString

exampleFinishedState = [r|{
    "state": "finished"
}|]

exampleRunningState = [r|{
    "state": "running",
    "data": [{
        "id": "dan",
        "x": 10,
        "y": 10,
        "vx": 10,
        "vy": 10,
        "theta": 10,
        "omega": 10,
        "Tl": 1,
        "Tr": -1
    }]
}|]

exampleShip = [r|{
    "id": "dan",
    "x": 10,
    "y": 10,
    "vx": 10,
    "vy": 10,
    "theta": 10,
    "omega": 10,
    "Tl": 1,
    "Tr": -1
}|]
