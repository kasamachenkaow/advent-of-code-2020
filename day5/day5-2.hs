import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)
import Data.List

maybeToInt :: Maybe Int -> Int
maybeToInt (Just n) = n
maybeToInt x = 0

replaceChar :: Char -> Char -> [Char] -> [Char]
replaceChar old new (x:xs) = (if x == old then new else x) : replaceChar old new xs
replaceChar old new xs = xs

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

toDecimalRowSeat :: (String, String) -> (Int, Int)
toDecimalRowSeat (row, seat) = (maybeToInt $ readBin row, maybeToInt $ readBin seat)

toSeatId :: (Int, Int) -> Int
toSeatId (row, seat) = row * 8 + seat

findMissingSeatId :: [Int] -> Int
findMissingSeatId (x1:x2:xs) = (if x1+1 == x2 then 0 else x1+1) + (findMissingSeatId (x2:xs))
findMissingSeatId xs = 0

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let replaceFwith0 = replaceChar 'F' '0'
  let replaceBwith1 = replaceChar 'B' '1'
  let replaceLwith0 = replaceChar 'L' '0'
  let replaceRwith1 = replaceChar 'R' '1'
  let replaceFBLR = replaceRwith1 . replaceLwith0 . replaceBwith1 . replaceFwith0
  let binaryInputs  = map replaceFBLR rawInputs
  let binaryRowAndSeat = map (splitAt 7) binaryInputs
  let decimalRowAndSeat = map toDecimalRowSeat binaryRowAndSeat
  let seatIds = map toSeatId decimalRowAndSeat
  let sortedSeatIds = sort seatIds
  let missingSeat = findMissingSeatId sortedSeatIds
  print missingSeat