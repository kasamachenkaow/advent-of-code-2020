import Data.List ()
import Data.List.Split

toInstructions :: [String] -> (String, Int)
toInstructions [c, '+' : xs] = (c, read xs :: Int)
toInstructions [c, a] = (c, read a :: Int)

getNewOffset :: (String, Int) -> Int -> Int
getNewOffset ("jmp", a) offset = offset + a
getNewOffset _ offset = offset + 1

getNextInstructions :: [(String, Int)] -> Int -> [Int] -> [(String, Int)]
getNextInstructions instructions offset history =
  if (offset `notElem` history) && (offset < length instructions)
    then instruction : getNextInstructions instructions newOffset (offset : history)
    else []
  where
    newOffset = getNewOffset instruction offset
    instruction = instructions !! offset

toArgument :: (String, Int) -> Int
toArgument (_, a) = a

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitInputs = map (splitOn " ") rawInputs -- [["com", "123"]]
  let instructions = map toInstructions splitInputs -- [("com", 123)]
  let nextInstructions = getNextInstructions instructions 0 []
  let accInstructions = [(i, a) | (i, a) <- nextInstructions, i == "acc"]
  let accArguments = map toArgument accInstructions
  let total = sum accArguments
  print total