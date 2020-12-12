import Data.List

emptyToZeros :: [[Int]] -> [[Int]]
emptyToZeros [] = [[0]]
emptyToZeros lists = lists

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just _) = 1

notZeroProduct :: [Int] -> Bool
notZeroProduct items = nonZero
  where
    nonZero = product items > 0

notEmptyArray :: [Int] -> Bool
notEmptyArray items = not (null items)

findProductOfPairOf :: ([Int], Int, Int) -> [Int]
findProductOfPairOf (lists, sumOf, 2) = do
  let pairs = map (sumOf -) lists
  let pairIndices = let fiLists n = elemIndex n lists in map fiLists pairs
  let pairIndexExists = map nothingToZero pairIndices
  let validPairs = transpose [lists, pairs, pairIndexExists]
  head (emptyToZeros (filter notZeroProduct validPairs))
findProductOfPairOf (lists, sumOf, itemCount) = do
  let findOthers n = n : findProductOfPairOf (filter (/= n) lists, sumOf - n, itemCount -1) in head (emptyToZeros (filter notZeroProduct (map findOthers lists)))

main :: IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let lists = let toI s = read s :: Int in map toI inputs
  let pairs = findProductOfPairOf (lists, 2020, 3)
  -- traceShowM "pairs"
  -- traceShowM pairs
  let result = product pairs
  print result