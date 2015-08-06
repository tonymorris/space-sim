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
    connect lobby "tcp://192.168.1.192:5558"
    state <- socket Sub
    connect state "tcp://192.168.1.192:5556"
    control <- socket Push
    connect state "tcp://192.168.1.192:5557"
    forever $ joinAndPlay lobby state control

joinAndPlay lobbyS stateS controlS = do
    liftIO $ print "joinAndPlay"
    send lobbyS [] (pack S.teamInfo)
    reply <- receive lobbyS
    let lobbyResponse = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName map secret) -> do
            -- load map
            -- mapData <- liftIO $ loadMap "/home/tmorris/Desktop/s/spacerace/maps" "swish"
            -- Wait for the game to begin
            --waitForGame (pack gameName) stateS
            gameLoop secret undefined stateS controlS

waitForGame gameName stateS = do
    liftIO $ print "waitForGame"
    [game, _state] <- receiveMulti stateS
    liftIO $ print (gameName, game)
    if gameName /= game
        then waitForGame gameName stateS
        else liftIO $ print "joining game"

gameLoop secret mapData stateS controlS = do
    {-state <- getCurrentState stateS
    case state of
        FinishedState -> liftIO $ print "finished game"
        RunningState ships -> do-}
            let command = toProtocol $ getCommand secret mapData undefined
            --liftIO $ print command
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
