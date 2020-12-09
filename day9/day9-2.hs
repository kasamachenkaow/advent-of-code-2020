import Data.List

toInt :: String -> Int
toInt s = read s :: Int

findContiguousSetOf :: [Int] -> Int -> Int -> [Int]
findContiguousSetOf list goal len
  | len > length list = []
  | sum candidates /= goal = findContiguousSetOf (tail list) goal len
  | otherwise = candidates
  where
    candidates = take len list

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  -- find contiguous set of 144381670
  let goal = 144381670
  let goalIndex = goal `elemIndex` numbers
  let numbersBefore = [x | x <- numbers, x `elemIndex` numbers < goalIndex]
  let nLength = length numbersBefore
  let contiguousSets = filter (not . null) [findContiguousSetOf numbersBefore goal len | len <- [2 .. nLength]]
  let contiguousSet = head contiguousSets
  print $ maximum contiguousSet + minimum contiguousSet