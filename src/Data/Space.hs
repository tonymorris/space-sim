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
import Data.ByteString.Char8(pack, unpack)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

main :: IO ()
main =
    runZMQ $ do
        sock <- socket Req
        connect sock "tcp://10.0.0.236:5558"
        send sock [] (pack lobby)
        reply <- receive sock
        let zz = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
        liftIO . print $ zz    
        case zz of 
          Nothing ->
            error "crash"
          Just (LobbyResponse n g m s) ->
            let y = toProtocol (controlexample s)
            in do send sock [] (pack y)
                  liftIO $ print y
        
controlexample ::
  String
  -> Control
controlexample k =
  Control k MainEngineOn AntiClock
