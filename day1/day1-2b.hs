import Data.List

nothingToZero::Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just n) = 1

findPairsOf::Int -> [Int] -> [[Int]]
findPairsOf sum (x:xs) = (
    let remaining = sum-x
    in [x, remaining, nothingToZero (elemIndex remaining xs)]
  )
  : findPairsOf sum xs
findPairsOf sum xs = []

findCombinationsOf::Int -> Int -> [Int] -> [[Int]]
findCombinationsOf sum 2 lists = findPairsOf sum lists
findCombinationsOf sum combinationNumber (x:xs) = concat [(
    let prependX l = x : l
    in map prependX $ findCombinationsOf (sum-x) (combinationNumber-1) xs
  ), (findCombinationsOf sum combinationNumber xs)]
findCombinationsOf sum combinationNumber xs = []

main::IO ()
main = do
  inputs <- lines <$> readFile "./input3"
  let lists = let toI s = read s :: Int in map toI inputs
  let findNCombinationsOf2020 = findCombinationsOf 2020
  let result = findNCombinationsOf2020 5 lists
  print $ filter (>0) (map product result)