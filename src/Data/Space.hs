{-# LANGUAGE OverloadedStrings #-}

module Data.Space(
  main
, module S
)where

import Data.Space.Control as S
import Data.Space.Map as S
import Data.Space.State as S
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
    connect lobby "tcp://10.0.0.236:5558"
    state <- socket Sub
    connect state "tcp://10.0.0.236:5556"
    control <- socket Push
    connect state "tcp://10.0.0.236:5557"

    forever $ joinAndPlay lobby state control

joinAndPlay lobbyS stateS controlS = do
    send lobbyS [] (pack S.teamInfo)
    reply <- receive lobbyS
    let lobbyResponse = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName map secret) -> do
            -- load map
            mapData <- undefined
            -- Wait for the game to begin
            waitForGame (pack gameName) stateS
            gameLoop secret mapData stateS controlS

waitForGame gameName stateS = do
    [game, _state] <- receiveMulti stateS
    if gameName /= game
        then waitForGame gameName stateS
        else return ()

gameLoop secret mapData stateS controlS = do
    state <- getCurrentState stateS
    case state of
        FinishedState -> return ()
        RunningState ships -> do
            let command = toProtocol $ getCommand secret mapData ships
            liftIO $ print command
            send controlS [] (pack command)
            gameLoop secret mapData stateS controlS

getCurrentState stateS = do
    [_gameName, state] <- receiveMulti stateS
    case decode (L.fromChunks [state]) of
        Just s -> return s
        Nothing -> error "could not parse game state"

getCommand secret mapData ships = Control secret MainEngineOn None

controlexample ::
  String
  -> Control
controlexample k =
  Control k MainEngineOn AntiClock
