module Spacerace.Map where

import Control.Applicative
import qualified Data.Vector as V
import System.FilePath.Posix ((</>))
import Prelude

data Flow a = Flow a a
  deriving (Show)

instance Functor Flow where
  fmap f (Flow a1 a2) =
    Flow (f a1) (f a2)

instance Applicative Flow where
  pure a =
    Flow a a
  Flow f1 f2 <*> Flow a1 a2 =
    Flow (f1 a1) (f2 a2)

data SpaceMap a = SpaceMap (V.Vector (V.Vector (Flow a)))
  deriving Show

instance Functor SpaceMap where
  fmap f (SpaceMap x) =
    SpaceMap ((fmap . fmap . fmap $ f) x)

instance Applicative SpaceMap where
  pure =
    SpaceMap . pure . pure . pure
  SpaceMap f <*> SpaceMap a =
    SpaceMap (liftA2 (liftA2 (<*>)) f a)

readTable :: String -> (V.Vector (V.Vector Double))
readTable =
  V.fromList . map (V.fromList . map read) . map words . lines

readMap :: String -> String -> SpaceMap Double
readMap x y = SpaceMap $ V.zipWith (V.zipWith Flow) tableX tableY
  where tableX = readTable x
        tableY = readTable y

loadMapPart :: FilePath -> [Char] -> [Char] -> [Char] -> IO String
loadMapPart dir m kind c = readFile $ dir</>m++"_"++kind++c++".csv"

loadFlow :: FilePath -> [Char] -> [Char] -> IO String
loadFlow dir m = loadMapPart dir m "flow"

loadMap :: FilePath -> String -> IO (SpaceMap Double)
loadMap dir m = do
  flowX <- loadFlow dir m "x"
  flowY <- loadFlow dir m "y"
  return $ readMap flowX flowY
