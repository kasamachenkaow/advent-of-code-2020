-- Bad performance, time complexity: O(n^2), Space O(n^2)

-- subListExists :: (Eq a) => [a] -> [a] -> Bool
-- subListExists subList (x:xs)
--   | subList `isPrefixOf` (x:xs) = True
--   | otherwise = subListExists subList xs
-- subListExists _ _ = False

import Data.List
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

toMultiplier :: Int -> Int
toMultiplier n = length allValids
  where
    differences = [ [combination !! i - combination !! (i-1) | i <- [1..length combination-1]] | combination <- subsequences [1..n+1], (not . null) combination, length combination >= 2, head combination == 1, last combination == n+1]
    allValids = [ maximum diff | diff <- differences, not $ null diff, maximum diff <= 3]

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