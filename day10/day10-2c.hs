-- Far better performance, time complexity: O(n), Space O(n)

import Data.List
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

trifib :: Int -> Int
trifib 1 = 1
trifib 2 = 1
trifib 3 = 2
trifib n = sum [trifib (n-1), trifib (n-2), trifib (n-3)]

toMultiplier :: Int -> Int
toMultiplier n = trifib (n+1)

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