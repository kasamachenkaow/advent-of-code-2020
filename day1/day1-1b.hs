import Data.List

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just _) = 1

findPairsOf :: Int -> [Int] -> [[Int]]
findPairsOf s (x : xs) =
  ( let remaining = s - x
     in [x, remaining, nothingToZero (elemIndex remaining xs)]
  ) :
  findPairsOf s xs
findPairsOf _ _ = []

main :: IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let lists = let toI s = read s :: Int in map toI inputs
  let findPairsOf2020 = findPairsOf 2020
  let result = findPairsOf2020 lists
  print $ filter (> 0) (map product result)