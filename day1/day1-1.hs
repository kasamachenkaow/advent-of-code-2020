import Data.List

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just _) = 1

main :: IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let lists = let toI s = read s :: Int in map toI inputs
  let pairs = map (2020 -) lists
  let pairIndices = let fiLists n = elemIndex n lists in map fiLists pairs
  let pairIndicesNum = map nothingToZero pairIndices
  let result = transpose [lists, pairs, pairIndicesNum]
  print $ filter (> 0) (map product result)