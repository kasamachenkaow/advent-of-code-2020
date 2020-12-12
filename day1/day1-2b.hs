import Data.List

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just _) = 1

findPairsOf :: Int -> [Int] -> [[Int]]
findPairsOf sum (x : xs) =
  ( let remaining = sum - x
     in [x, remaining, nothingToZero (remaining `elemIndex` xs)]
  ) :
  findPairsOf sum xs
findPairsOf _ _ = []

findCombinationsOf :: Int -> Int -> [Int] -> [[Int]]
findCombinationsOf sum 2 lists = findPairsOf sum lists
findCombinationsOf sum combinationNumber (x : xs) =
  ( let prependX l = x : l
     in map prependX $ findCombinationsOf (sum - x) (combinationNumber -1) xs
  )
    ++ findCombinationsOf sum combinationNumber xs
findCombinationsOf _ _ _ = []

main :: IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let lists = let toI s = read s :: Int in map toI inputs
  let find5CombinationsOf2020 = findCombinationsOf 2020 3
  let result = find5CombinationsOf2020 lists
  print $ filter (> 0) (map product result)