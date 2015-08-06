{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Space where

import Prelude
import System.ZMQ4.Monadic
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, unpack)
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Data.ByteString.Lazy(ByteString)

c = "{ \"name\": \"=<<\", \"team\": \"hi\" }"


data LobbyResponse =
  LobbyResponse {
    name :: String
  , game :: String
  , map :: String
  , secret :: String
  } deriving (Eq, Show, Generic, Typeable)

instance FromJSON LobbyResponse
  
main :: IO ()
main =
    runZMQ $ do
        reqSocket <- socket Req
        connect reqSocket "tcp://10.0.0.236:5558"
        send reqSocket [] (pack c)
        reply <- receive reqSocket
        liftIO $ putStrLn $ unwords ["Received reply:", unpack reply]    

example :: ByteString
example =
  "{\"game\":\"game13\",\"map\":\"starmap\",\"name\":\"=<<\",\"secret\":\"3e896b3a-9487\"}"