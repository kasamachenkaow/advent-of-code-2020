import Data.List

toInt :: String -> Int
toInt s = read s :: Int

findContiguousSetOf' :: [Int] -> Int -> [[Int]]
findContiguousSetOf' list goal = filter (\x -> sum x == goal)
  [
    [ list !! i | i <- [i..len+i-1] ]
      | len <- [2..length list]
      , i <- [0..length list - len]
      , Just i < goal `elemIndex` list
  ]

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  let goal = 144381670
  let contiguousSets = findContiguousSetOf' numbers goal
  let contiguousSet = head contiguousSets
  print $ maximum contiguousSet + minimum contiguousSet