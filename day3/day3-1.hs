import Data.List
import Data.List.Split

boolToInt::Bool -> Int
boolToInt True = 1
boolToInt False = 0

countTrees::Int -> [String] -> Int
countTrees pos (m:ms) = boolToInt (m !! pos == '#') + (countTrees ((pos + 3) `mod` (length m)) ms)
countTrees pos ms = 0

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  print $ countTrees 0 rawInputs