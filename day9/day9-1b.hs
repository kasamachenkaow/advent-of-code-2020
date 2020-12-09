toInt :: String -> Int
toInt s = read s :: Int

getPreambles :: Int -> [Int] -> [(Int, [Int])]
getPreambles preamNum list = [(list !! i, [ list !! p | p <- [i-preamNum..i]]) | i <- [preamNum..length list]]

toPairSums :: (Int, [Int]) -> (Int, [Int])
toPairSums (curr, preamble) = (curr, map sum pairs)
  where
    pairs = [ [preamble !! i1, preamble !! i2] | i1 <- [0..pLength-1], preamble !! i1 < curr, i2 <- [0..i1], preamble !! i2 < curr]
    pLength = length preamble

getInvalid :: [(Int, [Int])] -> [Int]
getInvalid list = [curr | (curr, pairSums) <- list, curr `notElem` pairSums]

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  let preambles = getPreambles 25 numbers
  let preamblesWithPairs = map toPairSums preambles
  let firstInvalid = (head . getInvalid) preamblesWithPairs
  print firstInvalid