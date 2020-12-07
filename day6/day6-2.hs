import Data.List
import Data.List.Split

intersectCharList :: [[Char]] -> [Char]
intersectCharList (x : xs) = x `intersect` intersectCharList xs
intersectCharList _ = ['a' .. 'z']

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let groupArraysByEmptyLine = splitOn [""] rawInputs
  let sharedQuestionsGroupss = map intersectCharList groupArraysByEmptyLine
  let sharedQuestionsGroupCounts = map length sharedQuestionsGroupss
  let sumCount = sum sharedQuestionsGroupCounts
  print sumCount