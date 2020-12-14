import Common
import Data.List
import Data.List.Split
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits (bit)

wears :: String -> String -> String
wears (b:bs) (m:ms)
  | m == '0' = b : wears bs ms
  | m == '1' = m : wears bs ms
  | m == 'X' = m : wears bs ms
wears bs (m:ms) = m : wears bs ms
wears _ _ = ""

stringBitsToInt :: String -> Int
stringBitsToInt list = sum [ bit bi :: Int | bi <- [0..length list-1], list !! bi == '1' ]

floatingToBits :: String -> [String]
floatingToBits (x:xs)
  | x == 'X' = [ b:rest | rest <- floatingToBits xs, b <- ['0', '1']]
  | otherwise = [ x:rest | rest <- floatingToBits xs]
floatingToBits _ = [""]

initialize :: [(String, String)] -> String -> [(Int, Int)]
initialize (("mask", argument):xs) _ = initialize xs argument
initialize ((mem, argument):xs) mask = newMems ++ initialize xs mask
  where
    newMems = newAddrs `zip` (replicate . length) newAddrs value
    value = toInt argument
    newAddrs = (map stringBitsToInt . floatingToBits) newBitStrings -- "X1X1" -> ["0101", "1101", "0111", "1111"] -> [10, 11, 14, 15]
    newBitStrings = rbits `wears` rmask -- "0111" `wears` "X0X1" -> "X1X1"
    rbits = reverse argumentBitStrings
    rmask = reverse mask
    argumentBitStrings = showIntAtBase 2 intToDigit (toInt addr) "" -- "14" -> "1110"
    addr = init ms
    ('m':'e':'m':'[':ms) = mem
initialize _ _ = []


main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let instructions = map (toTuple . splitOn " = ") rawInputs
  let mems = initialize instructions ""
  let keyValues = unzip mems
  let keys = (reverse . fst) keyValues
  let values = (reverse . snd) keyValues
  let uniqueValues = [ values !! i | i <- [0..(length keys-1)], ((keys !! i) `elemIndex` keys) == Just i ]
  print $ instructions
  -- print $ mems
  -- print $ keyValues
  -- print $ uniqueValues
  print $ sum uniqueValues
