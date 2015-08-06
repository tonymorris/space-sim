{-# LANGUAGE OverloadedStrings #-}

module Data.Space(
  main
, module S
)where

import Data.Space.Control as S
import Data.Space.Map as S
import Data.Space.State as S
import Data.Space.Lobby as S
import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack)
import Data.Aeson
import qualified Data.ByteString.Lazy as L

main :: IO ()
main =
    runZMQ $ do
        sock <- socket Req
        connect sock "tcp://10.0.0.236:5558"
        send sock [] (pack lobby)
        reply <- receive sock
        case decode (L.fromChunks [reply]) of 
          Nothing ->
            error "crash"
          Just (LobbyResponse _ _ _ s) ->
            let ctrl = controlexample s
            in do sendP sock [] ctrl
                  liftIO $ print ctrl
        
controlexample ::
  String
  -> Control
controlexample k =
  Control k MainEngineOn AntiClock
