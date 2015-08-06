module Data.Space.Map where


import System.FilePath.Posix ((</>))
import Prelude

data Flow a = Flow a a
  deriving (Show)

instance Functor Flow where
  fmap f (Flow a1 a2) =
    Flow (f a1) (f a2)
    
data SpaceMap a = SpaceMap [[Flow a]]
  deriving (Show)

readTable :: String -> [[Double]]
readTable = map (map read) . map words . lines

readMap :: String -> String -> SpaceMap Double
readMap x y = SpaceMap $ zipWith (zipWith Flow) tableX tableY
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
