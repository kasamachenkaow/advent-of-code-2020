import Data.List ()
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

toTuple :: [String] -> (String, String, String)
toTuple [rules, char, text] = (rules, char, text)

sanitizeInput :: (String, String, String) -> ([Int], Char, String)
sanitizeInput (rawRules, rawChar, text) = (rules, char, text)
  where
    rules = map toInt stringRules
    stringRules = splitOn "-" rawRules
    char = head rawChar

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor True True = False
xor False False = False

validate :: ([Int], Char, String) -> Bool
validate ([index1, index2], char, text) = xor ((text !! (index1 -1)) == char) ((text !! (index2 -1)) == char)

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let stringInputs = map words rawInputs
  let tupleInputs = map toTuple stringInputs
  let sanitizedInputs = map sanitizeInput tupleInputs
  let validatedResult = map validate sanitizedInputs
  let validCount = length $ filter (== True) validatedResult
  print $ validCount