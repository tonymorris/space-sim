module Data.Space.Map where

import System.IO (readFile)
import System.FilePath.Posix ((</>))
import Prelude

data Flow a = Flow a a
  deriving (Show)
data SpaceMap a = SpaceMap [[Flow a]]
  deriving (Show)

readTable = map (map read) . map words . lines

readMap x y = SpaceMap $ zipWith (zipWith Flow) tableX tableY
  where tableX = readTable x
        tableY = readTable y

loadMapPart dir map kind c = readFile $ dir</>map++"_"++kind++c++".csv"

loadFlow dir map = loadMapPart dir map "flow"

loadMap :: FilePath -> String -> IO (SpaceMap Double)
loadMap dir map = do
  flowX <- loadFlow dir map "x"
  flowY <- loadFlow dir map "y"
  return $ readMap flowX flowY
