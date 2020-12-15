-- O(n^2)
import Common
import Data.List
import Data.Maybe

runMemoryGame :: [Int] -> Int -> Int -> [Int]
runMemoryGame (x:xs) max count
  | max == count = x:xs
  | isNothing i = runMemoryGame (0:x:xs) max (count+1)
  | otherwise = runMemoryGame (fromJust i + 1:x:xs) max (count+1)
  where
    i = x `elemIndex` xs

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input2"
  let inputs = toListInt $ head rawInputs
  let rInputs = reverse inputs
  let results = runMemoryGame rInputs (2020-length rInputs) 0
  print $ inputs
  print $ rInputs
  -- print $ results
  print $ head results