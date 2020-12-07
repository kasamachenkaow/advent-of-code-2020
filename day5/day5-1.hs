import Data.Char (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric (readInt)

maybeToInt :: Maybe Int -> Int
maybeToInt (Just n) = n
maybeToInt _ = 0

replaceChar :: Char -> Char -> [Char] -> [Char]
replaceChar old new (x : xs) = (if x == old then new else x) : replaceChar old new xs
replaceChar _ _ xs = xs

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

toDecimalRowSeat :: (String, String) -> (Int, Int)
toDecimalRowSeat (row, seat) = (maybeToInt $ readBin row, maybeToInt $ readBin seat)

toSeatId :: (Int, Int) -> Int
toSeatId (row, seat) = row * 8 + seat

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let replaceFwith0 = replaceChar 'F' '0'
  let replaceBwith1 = replaceChar 'B' '1'
  let replaceLwith0 = replaceChar 'L' '0'
  let replaceRwith1 = replaceChar 'R' '1'
  let replaceFBLR = replaceRwith1 . replaceLwith0 . replaceBwith1 . replaceFwith0
  let binaryInputs = map replaceFBLR rawInputs
  let binaryRowAndSeat = map (splitAt 7) binaryInputs
  let decimalRowAndSeat = map toDecimalRowSeat binaryRowAndSeat
  let seatIds = map toSeatId decimalRowAndSeat
  let maxId = maximum seatIds
  print maxId