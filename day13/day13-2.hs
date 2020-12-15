import Common
import Data.List
import Data.List.Split

xTo1 :: String -> String
xTo1 "x" = "1"
xTo1 n = n

validate :: [[Int]] -> Int -> Int -> Bool
validate list maxI num = length [ True | i <- [0..length list-1], ((num-(maxI - last (list !! i))) `mod` head (list !! i)) == 0 ] == length list

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input3"
  let lines = map (splitOn ",") rawInputs
  let linesWith1 = map (map (toInt . xTo1)) lines
  let linesWithIndex = [ (reverse . sort) [[buses !! i, i] | i <- [0..length buses-1], buses !! i /= 1] | buses <- linesWith1 ]
  -- 10^11
  let anwsers = [head [ m - (last . head) buses | m <- [(head . head) buses*(10^0), (head . head) buses*(10^0+1)..], validate buses ((last . head) buses) m ] | buses <- linesWithIndex ]
  print $ lines
  print $ linesWith1
  print $ linesWithIndex
  print $ anwsers