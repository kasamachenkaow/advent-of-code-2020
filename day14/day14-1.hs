import Common
import Data.List
import Data.List.Split
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits (bit)

wears :: String -> String -> String
wears (b:bs) (m:ms)
  | m == '0' = m : wears bs ms
  | m == '1' = m : wears bs ms
  | m == 'X' = b : wears bs ms
wears bs (m:ms)
  | m == 'X' = '0' : wears bs ms
  | otherwise = m : wears bs ms
wears _ _ = ""


initialize :: [(String, String)] -> String -> [(String, Int)]
initialize (("mask", argument):xs) _ = initialize xs argument
initialize ((mem, argument):xs) mask = (mem, newValue) : initialize xs mask
  where
    newValue = sum [ bit bi :: Int | bi <- [0..length newBitStrings-1], newBitStrings !! bi == '1' ] -- "1100" -> 1100 -> 12
    newBitStrings = rbits `wears` rmask -- "1110" `wears` "1X0X" -> "1100"
    rbits = reverse argumentBitStrings
    rmask = reverse mask
    argumentBitStrings = showIntAtBase 2 intToDigit (toInt argument) "" -- "14" -> "1110"
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
  print $ mems
  print $ keyValues
  print $ uniqueValues
  print $ sum uniqueValues
  -- let argumentBitStrings = showIntAtBase 2 intToDigit (toInt "101") ""
  -- let mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  -- let rbits = reverse argumentBitStrings
  -- let rmask = reverse mask
  -- let newBitStrings = rbits `wears` rmask
  -- let newValue = sum [ bit bi :: Int | bi <- [0..length newBitStrings-1], newBitStrings !! bi == '1' ]
  -- print $ argumentBitStrings
  -- print $ rmask
  -- print $ rbits
  -- print $ newBitStrings
  -- print $ newValue
