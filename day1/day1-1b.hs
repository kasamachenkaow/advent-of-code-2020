import Data.List

nothingToZero::Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just n) = 1

findPairsOf::Int -> [Int] -> [[Int]]
findPairsOf s (x:xs) = (
    let remaining = s-x
    in [x, s-x, nothingToZero (elemIndex (s-x) xs)]
  )
  : findPairsOf s xs
findPairsOf s xs = []

main::IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let lists = let toI s = read s :: Int in map toI inputs
  let findPairsOf2020 = findPairsOf 2020
  let result = findPairsOf2020 lists
  print $ filter (>0) (map product result)