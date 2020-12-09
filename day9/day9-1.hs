import Data.List

toInt :: String -> Int
toInt s = read s :: Int

getPreambleList :: [Int] -> Int -> [(Int, [Int])]
getPreambleList list offset
  | offset < 25 = []
  | offset >= length list = []
  | otherwise = (curr, preamble) : getPreambleList list (offset + 1)
  where
    curr = list !! offset
    preamble = take 25 (drop (offset - 25) list)

getSumPairsOf :: [Int] -> [Int]
getSumPairsOf (x : xs) = sumPair ++ getSumPairsOf xs
  where
    sumPair = map sum (transpose [first, xs])
    first = replicate (length xs) x
getSumPairsOf _ = []

toPairs :: (Int, [Int]) -> (Int, [Int])
toPairs (curr, preamble) = (curr, pairs)
  where
    pairs = getSumPairsOf preamble

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  let preambles = getPreambleList numbers 25
  let preamblesWithPairs = map toPairs preambles
  let firstInvalid = head [curr | (curr, pairSums) <- preamblesWithPairs, curr `notElem` pairSums]
  print firstInvalid