{-# LANGUAGE OverloadedStrings #-}

module Data.Space(
  main
, module S
)where

import Data.Space.Control as S
import Data.Space.Map as S
import Data.Space.Lobby as S
import Data.Space.State as S
import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad

main :: IO ()
main = runZMQ $ do
    lobby <- socket Req
    connect lobby "tcp://192.168.1.192:5558"
    state <- socket Sub
    connect state "tcp://192.168.1.192:5556"
    control <- socket Push
    connect state "tcp://192.168.1.192:5557"
    forever $ joinAndPlay lobby state control

joinAndPlay ::
  (Sender t, Sender s, Receiver t, Receiver r) =>
  Socket z t
  -> Socket z r
  -> Socket z s
  -> ZMQ z ()
joinAndPlay lobbyS stateS controlS = do
    send lobbyS [] (pack S.teamInfo)
    reply <- receive lobbyS
    let lobbyResponse = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName _ s) -> do
            -- load map
            m <- liftIO $ loadMap "/home/tmorris/Desktop/r/spacerace/maps" "swish"
            -- Wait for the game to begin
            waitForGame (pack gameName) stateS
            gameLoop s m stateS controlS

waitForGame :: 
  Receiver t =>
  B.ByteString
  -> Socket z t
  -> ZMQ z ()
waitForGame gameName stateS = do
    [g, _state] <- receiveMulti stateS
    when (gameName /= g) (waitForGame gameName stateS)
    
gameLoop ::
  (Sender s, Receiver t) =>
  String
  -> SpaceMap Double
  -> Socket z t
  -> Socket z s
  -> ZMQ z ()
gameLoop s m stateS controlS = do
    state <- getCurrentState stateS
    case state of
        FinishedState -> return ()
        RunningState ships -> do
            let command = toProtocol $ getCommand s m ships
            liftIO $ print command
            send controlS [] (pack command)
            gameLoop s m stateS controlS

getCurrentState ::  (FromJSON b, Receiver t) => Socket z t -> ZMQ z b
getCurrentState stateS = do
    [_gameName, state] <- receiveMulti stateS
    case decode (L.fromChunks [state]) of
        Just s -> return s
        Nothing -> error "could not parse game state"

getCommand :: String -> SpaceMap Double -> [Ship] -> Control
getCommand s _ _ = Control s MainEngineOn None
