{-# LANGUAGE DeriveGeneric #-}

module Data.Space.Lobby(
  lobby
, LobbyResponse(..)
) where

import Prelude
import GHC.Generics
import Data.Typeable
import Data.Aeson

lobby ::
  String
lobby =
  "{ \"name\": \"=<<\", \"team\": \"hi\" }"

data LobbyResponse =
  LobbyResponse {
    name :: String
  , game :: String
  , map :: String
  , secret :: String
  } deriving (Eq, Show, Generic, Typeable)

instance FromJSON LobbyResponse
