import Data.List
import Data.List.Split

getKeys::String -> String
getKeys keyValue = take 3 keyValue

-- TO-DO: Find the better way to not need to sort first
validate::[String] -> Bool
validate ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
validate ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] = True
validate xs = False

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitArrayInput = splitOn [""] rawInputs
  let mergeSplitArray = map unwords splitArrayInput
  let splitKeyValueArray = map (splitOn " ") mergeSplitArray
  let passportKeysArray = map (map getKeys) splitKeyValueArray
  let sortedPassportKeysArray = map sort passportKeysArray
  let validatedResult = map validate sortedPassportKeysArray
  let validCount = length $ filter (==True) validatedResult
  print $ validCount