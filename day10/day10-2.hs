-- Time complexity O(n), Space O(n)

import Data.List
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

toMultiplier :: Int -> Int
toMultiplier 1 = 1
toMultiplier 2 = 2
toMultiplier 3 = 4
toMultiplier 4 = 7
toMultiplier 5 = 13
toMultiplier 6 = 24
toMultiplier _ = 0

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  let outletJoltage = 0
  let adapterJoltages = sort numbers
  let deviceJoltage = last adapterJoltages + 3
  let allJoltages = [outletJoltage] ++ adapterJoltages ++ [deviceJoltage]
  let differences = [ allJoltages !! i - allJoltages !! (i-1) | i <- [1..length allJoltages-1]]
  let groupOf1s = filter (not . null) (splitOn [3] differences)
  let groupOf1sCount = [ length ones | ones <- groupOf1s]
  let mulipliers = map toMultiplier groupOf1sCount
  print $ allJoltages
  print $ differences
  print $ groupOf1s
  print $ groupOf1sCount
  print $ mulipliers
  print $ product mulipliers

-- Input2 : 8
-- Input3 : 19208