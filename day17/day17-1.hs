import Common
import Data.List ( transpose )
import Data.Map (Map)
import qualified Data.Map as Map

toKey :: Int -> Int -> Int -> String
toKey x y z = show x ++ "," ++ show y ++ "," ++ show z

-- getPointsFromLevel :: Int -> [(x,y,z)]
-- getPointsFromLevel level = []

-- getNeighbors :: Map String Char -> (x,y,z) -> [Char]
-- getNeighbors map centerPoint = []

-- transform :: ??
  -- start from the origin [0,0,0]
  -- each level get points using `getPointsFromLevel()`
  -- transform all the points using `getNeighbors()` check with rules
  -- loop to N level (N = the outmost level)

transform :: Map String Char -> Int -> Map String Char
transform universe cycle
  | 

loopResult :: (a -> a) -> a -> Int
loopResult f arg round
  | round > 0 = loopResult f (f arg) (round-1)
  | otherwise = a

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let input = transpose rawInputs
  let yLength = length rawInputs
  let xLength = length (head rawInputs)
  let yRadius = yLength `div` 2
  let xRadius = xLength `div` 2
  -- creat the init using x,y,z as a key and shift x,y to origin [0,0,0]
  let points = Map.fromList [(toKey (x-xRadius) (y-yRadius) 0, input !! x !! y) | x <- [0..xLength-1], y <- [0..yLength-1]]
  -- check the outmost level of init, attach it to the map as well
  let universe = (xRadius, points)
  -- `transform()` N rounds
  -- get all points from the latest transformed result
  -- count '#'
  print rawInputs
  print input
  print universe