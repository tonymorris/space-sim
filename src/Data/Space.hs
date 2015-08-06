{-# LANGUAGE OverloadedStrings #-}

module Data.Space {-(
  main
, module S
) -} where

import Data.Space.Control as S
import Data.Space.Map as S
import Data.Space.Lobby as S
import Data.Space.State as S
import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack)
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad

data Config =
  Config
    String -- host
    String -- request port
    String -- sub port
    String -- push port
    FilePath -- maps
  deriving (Eq, Show)

main :: IO ()
main = 
  run (Config "192.168.1.192" "5558" "5556" "5557" "/home/tmorris/Desktop/r/spacerace/maps")

run :: Config -> IO ()
run (Config c rp sp pp mapdir) =
  let connectstring p = "tcp://" ++ c ++ ":" ++ p
      connectto s = connect s . connectstring
  in
    runZMQ $ do
      lobby <- socket Req
      connectto lobby rp
      state <- socket Sub
      connect state sp
      control <- socket Push
      connect state pp
      forever $ joinAndPlay lobby state control mapdir

joinAndPlay ::
  (Sender t, Sender s, Receiver t, Receiver r) =>
  Socket z t
  -> Socket z r
  -> Socket z s
  -> FilePath
  -> ZMQ z ()
joinAndPlay lobbyS stateS controlS mapdir = do
    send lobbyS [] (pack S.teamInfo)
    reply <- receive lobbyS
    let lobbyResponse = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName _ s) -> do
            -- load map
            m <- liftIO $ loadMap mapdir "swish"
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

getCurrentState ::
  (FromJSON a, Receiver t) =>
  Socket z t
  -> ZMQ z a
getCurrentState stateS = do
    [_gameName, state] <- receiveMulti stateS
    case decode (L.fromChunks [state]) of
        Just s -> return s
        Nothing -> error "could not parse game state"

getCommand :: String -> SpaceMap Double -> [Ship] -> Control
getCommand s _ _ = Control s MainEngineOn None

data Error =
  DeleteError String

data Log =
  Log
    String

newtype SpaceT f a =
  SpaceT
    (Config -> f ([Log], Either Error a))

instance Functor f => Functor (SpaceT f) where
 fmap f (SpaceT s) =
    SpaceT (fmap (fmap (fmap f)) . s)

instance Applicative f => Applicative (SpaceT f) where
  pure =
    SpaceT . pure . pure . pure . pure
  SpaceT f <*> SpaceT a =
    SpaceT ((liftA2 . liftA2 . liftA2) (<*>) f a)

instance Monad f => Monad (SpaceT f) where
  return =
    SpaceT . return . return . (,) [] . return 
  SpaceT s >>= f =
    SpaceT
      (\c -> s c >>= \(l, e) -> case e of
                                  Left r -> return (l, Left r)
                                  Right a -> let SpaceT t = f a
                                             in t c >>= \(m, q) -> return (l ++ m, q))

type ZSpace z a =
  SpaceT (ZMQ z) a

-- type Space a = SpaceT Identity a
