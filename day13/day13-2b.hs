import Common
import Data.List
import Data.List.Split

xTo1 :: String -> String
xTo1 "x" = "1"
xTo1 n = n

validate :: [[Int]] -> Int -> Bool
validate ([x,xi]:xs) n
  | (n+xi) `mod` x == 0 = validate xs n
  | otherwise = False
validate _ _ = True

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input2"
  let lines = map (splitOn ",") rawInputs
  let linesWith1 = map (map (toInt . xTo1)) lines
  let linesWithIndex = [ (reverse . sort) [[buses !! i, i] | i <- [0..length buses-1], buses !! i /= 1] | buses <- linesWith1 ]
    -- 10^11
  let anwsers = [head [ m - (last . head) buses | m <- [(head . head) buses*(10^11), (head . head) buses*(10^11+1)..], validate buses (m - (last . head) buses) ] | buses <- linesWithIndex ]
  print $ lines
  print $ linesWith1
  print $ linesWithIndex
  print $ anwsers