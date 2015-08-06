{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Space.Lobby(
  teamInfo
, LobbyResponse(..)
, AsLobbyResponse(..)
, AsName(..)
, AsGame(..)
, AsMap(..)
, AsSecret(..)
) where

import Control.Category(Category(id))
import Control.Lens(Optic', lens)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.String(String)
import Data.Typeable(Typeable)
import Data.Aeson(FromJSON)
import GHC.Generics(Generic)
import Prelude(Show)

teamInfo ::
  String
teamInfo =
  "{ \"name\": \"=<<\", \"team\": \"tony\" }"

data LobbyResponse =
  LobbyResponse {
    name :: String
  , game :: String
  , map :: String
  , secret :: String
  } deriving (Eq, Show, Generic, Typeable)

instance FromJSON LobbyResponse

class AsLobbyResponse p f s where
  _LobbyResponse ::
    Optic' p f s LobbyResponse

instance AsLobbyResponse p f LobbyResponse where
  _LobbyResponse =
    id

class AsName p f s where
  _Name ::
    Optic' p f s String

instance AsName p f String where
  _Name =
    id

instance Functor f => AsName (->) f LobbyResponse where
  _Name =
    lens
      (\(LobbyResponse n _ _ _) -> n)
      (\(LobbyResponse _ g m s) n -> LobbyResponse n g m s)

class AsGame p f s where
  _Game ::
    Optic' p f s String

instance AsGame p f String where
  _Game =
    id

instance Functor f => AsGame (->) f LobbyResponse where
  _Game =
    lens
      (\(LobbyResponse _ g _ _) -> g)
      (\(LobbyResponse n _ m s) g -> LobbyResponse n g m s)

class AsMap p f s where
  _Map ::
    Optic' p f s String

instance AsMap p f String where
  _Map =
    id

instance Functor f => AsMap (->) f LobbyResponse where
  _Map =
    lens
      (\(LobbyResponse _ _ m _) -> m)
      (\(LobbyResponse n g _ s) m -> LobbyResponse n g m s)
      
class AsSecret p f s where
  _Secret ::
    Optic' p f s String

instance AsSecret p f String where
  _Secret =
    id

instance Functor f => AsSecret (->) f LobbyResponse where
  _Secret =
    lens
      (\(LobbyResponse _ _ _ s) -> s)
      (\(LobbyResponse n g m _) s -> LobbyResponse n g m s)
      
