import Data.List
import Data.List.Split
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

getKeys :: String -> String
getKeys = take 3

toKeyValueTuple :: String -> (String, String)
toKeyValueTuple (x : y : z : ':' : xs) = ([x, y, z], xs)

validateRequiredKeys :: [String] -> Bool
validateRequiredKeys ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
validateRequiredKeys ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
validateRequiredKeys xs = False

validateValue :: (String, String) -> Bool
validateValue ("byr", value) = matchTest regex value
  where
    regex = makeRegex "^(19[2-9][0-9]|200[0-2])$" :: Regex
validateValue ("cid", _) = True
validateValue ("ecl", value) = matchTest regex value
  where
    regex = makeRegex "^(amb|blu|brn|gry|grn|hzl|oth)$" :: Regex
validateValue ("eyr", value) = matchTest regex value
  where
    regex = makeRegex "^20([2][0-9]|30)$" :: Regex
validateValue ("hcl", value) = matchTest regex value
  where
    regex = makeRegex "^#[0-9a-f]{6}$" :: Regex
validateValue ("hgt", value) = matchTest regex value
  where
    regex = makeRegex "^(1[5-8][0-9]cm|19[0-3]cm|5[89]in|6[0-9]in|7[0-6]in)$" :: Regex
validateValue ("iyr", value) = matchTest regex value
  where
    regex = makeRegex "^20([1][0-9]|20)$" :: Regex
validateValue ("pid", value) = matchTest regex value
  where
    regex = makeRegex "^[0-9]{9}$" :: Regex
validateValue (_, _) = False

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitArrayInput = splitOn [""] rawInputs
  let mergeSplitArray = map unwords splitArrayInput
  let splitKeyValueArray = map (splitOn " ") mergeSplitArray
  -- check required keys
  let passportKeysArray = map (map getKeys) splitKeyValueArray
  let sortedPassportKeysArray = map sort passportKeysArray
  let requiredKeysResult = map validateRequiredKeys sortedPassportKeysArray
  -- check valid values
  let passportKeyValuesArray = map (map toKeyValueTuple) splitKeyValueArray
  let validValuesResult = map (map validateValue) passportKeyValuesArray
  let allValidValuesResult = map (all (== True)) validValuesResult
  -- merge two results
  let validRequireKeysValueResult = map (all (== True)) $ transpose [allValidValuesResult, requiredKeysResult]
  -- print final result
  let validCount = length $ filter (== True) validRequireKeysValueResult
  print $ validCount