import Data.List
import Debug.Trace


emptyToZeros::[[Int]] -> [[Int]]
emptyToZeros [] = [[0]]
emptyToZeros lists = lists

nothingToZero::Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just n) = 1

notZeroProduct::[Int] -> Bool
notZeroProduct items = nonZero where
  nonZero = (product items) > 0

notEmptyArray::[Int] -> Bool
notEmptyArray items = not (null items)

findProductOfPairOf::([Int], Int, Int) -> [Int]
findProductOfPairOf (lists, sumOf, 2) = do
  let pairs = map (sumOf-) lists
  let pairIndices =  let fiLists n = elemIndex n lists in map fiLists pairs
  let pairIndexExists = map nothingToZero pairIndices
  let validPairs = transpose [lists, pairs, pairIndexExists]
  result <- head (emptyToZeros (filter notZeroProduct validPairs))
  return result
findProductOfPairOf (lists, sumOf, itemCount) = do
  result <- let findOthers n = n : (findProductOfPairOf (filter (\p -> p /= n) lists, sumOf-n, itemCount-1)) in head (emptyToZeros (filter notZeroProduct (map findOthers lists)))
  return result

main::IO ()
main = do
  inputs <- lines <$> readFile "./input3"
  let lists = let toI s = read s :: Int in map toI inputs
  let pairs = (findProductOfPairOf (lists, 2020, 5))
  -- traceShowM "pairs"
  -- traceShowM pairs
  let result = product pairs
  print result