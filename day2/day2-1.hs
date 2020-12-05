import Data.List
import Data.List.Split

toInt :: String -> Int
toInt s = read s :: Int

toTuple :: [String] -> (String, String, String)
toTuple [rules,char,text] = (rules, char, text)

sanitizeInput :: (String, String, String) -> ([Int], Char, String)
sanitizeInput (rawRules, rawChar, text) = (rules, char, text) where
  rules = map toInt stringRules
  stringRules = splitOn "-" rawRules
  char = head rawChar

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

getRulesWithCount :: ([Int], Char, String) -> ([Int], Int)
getRulesWithCount (rules, char, text) = (rules, (countLetters text char))

validate :: ([Int], Int) -> Bool
validate ([min,max], count) = (count >= min) && (count <= max)

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let stringInputs = map words rawInputs
  let tupleInputs = map toTuple stringInputs
  let sanitizedInputs = map sanitizeInput tupleInputs
  let rulesWithCount = map getRulesWithCount sanitizedInputs
  let validatedResult = map validate rulesWithCount
  let validCount = length $ filter (==True) validatedResult
  print $ validCount