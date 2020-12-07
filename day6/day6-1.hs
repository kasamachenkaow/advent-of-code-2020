import Data.List
import Data.List.Split

unionCharList :: [[Char]] -> [Char]
unionCharList (x : xs) = x `union` unionCharList xs
unionCharList _ = []

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let groupArraysByEmptyLine = splitOn [""] rawInputs
  let uniqueQuestionsGroupss = map unionCharList groupArraysByEmptyLine
  let uniqueGroupCounts = map length uniqueQuestionsGroupss
  let sumCount = sum uniqueGroupCounts
  print sumCount