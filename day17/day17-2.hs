import Common
import Data.List ( transpose, elemIndices )
import Data.Map (Map, mapAccum, findWithDefault)
import qualified Data.Map as Map

findWithDefaultInactive = findWithDefault '.'

toKey :: (Int, Int, Int, Int) -> String
toKey (x, y, z, w) = show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w

getPointsFromLevel :: Int -> [(Int, Int, Int, Int)]
getPointsFromLevel level = [(x, y, z, w) | x <- [-level..level], y <- [-level..level], z <- [-level..level], w <- [-level..level]]

getNeighbors :: Map String Char -> (Int, Int, Int, Int) -> [Char]
getNeighbors map (x, y, z, w) = [ findWithDefaultInactive (toKey (x',y',z',w')) map | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1], x' /= x || y' /= y || z' /= z || w' /= w]

tryActivate :: Char -> [Char] -> Char
tryActivate '#' neighbors
  | actives >= 2 && actives <= 3 = '#'
  | otherwise = '.'
  where actives = length $ '#' `elemIndices` neighbors
tryActivate '.' neighbors
  | actives == 3 = '#'
  | otherwise = '.'
  where actives = length $ '#' `elemIndices` neighbors

transform :: (Int, Map String Char) ->  (Int, Map String Char)
transform (level, universe) = (level+1, newUniverse)
  where
    newUniverse = Map.fromList [
      let key = toKey p in 
        (key, ((tryActivate (findWithDefaultInactive key universe)) . getNeighborsWithMap) p) 
        | p <- getPointsFromLevel level
      ]
    getNeighborsWithMap = getNeighbors universe
    
loopResult :: (a -> a) -> a -> Int -> a
loopResult f arg round
  | round > 0 = loopResult f (f arg) (round-1)
  | otherwise = arg

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let input = transpose rawInputs
  let yLength = length rawInputs
  let xLength = length (head rawInputs)
  let yRadius = yLength `div` 2
  let xRadius = xLength `div` 2
  -- creat the init using x,y,z as a key and shift x,y to origin [0,0,0]
  let points = Map.fromList [(toKey (x-xRadius, y-yRadius, 0, 0), input !! x !! y) | x <- [0..xLength-1], y <- [0..yLength-1]]
  -- check the outmost level of init, attach it to the map as well
  let maxLevel = (ceiling (fromIntegral (xLength-1) / 2))+1
  let universe = (maxLevel, points)
  let newUniverse = loopResult transform universe 6
  -- `transform()` N rounds
  -- get all points from the latest transformed result
  -- count '#'
  let f a b = (a+(if b == '#' then 1 else 0), b)
  let allActiveCount = fst $ mapAccum f 0 (snd newUniverse)
  print rawInputs
  print input
  print maxLevel
  print universe
  -- print newUniverse
  print allActiveCount
  -- debug
  -- print $ getPointsFromLevel 2
  -- print $ findWithDefaultInactive (toKey (-1,0,0)) (snd universe)
  -- print $ getNeighbors (snd universe) (-1,0,0)