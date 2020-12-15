-- O(n logn)
import Common
import Data.Map (Map)
import qualified Data.Map as Map

runMemoryGame :: Map Int Int -> Int -> Int -> Int -> Int
runMemoryGame posMap max count lastn
  | max == count = lastn
  | otherwise = (runMemoryGame $! Map.insert lastn count posMap) max (count+1) nextn
  where
    nextn =  case Map.lookup lastn posMap of
                  Nothing  -> 0
                  Just i -> count - i

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let inputs = toListInt $ head rawInputs
  let posMap = Map.fromList [(inputs !! i,i+1) | i <- [0..length inputs-1]]
  let lastn = last inputs
  let lastPosMap = Map.delete lastn posMap
  let result = runMemoryGame lastPosMap 30000000 (length inputs) lastn
  print $ inputs
  print $ posMap
  print $ lastn
  print $ lastPosMap
  print $ result