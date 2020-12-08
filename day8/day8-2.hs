import Data.List ()
import Data.List.Split

toInstructions :: [String] -> (String, Int)
toInstructions [c, '+' : xs] = (c, read xs :: Int)
toInstructions [c, a] = (c, read a :: Int)

getNewOffset :: (String, Int) -> Int -> Int
getNewOffset ("jmp", a) offset = offset + a
getNewOffset _ offset = offset + 1

getNextInstructions :: [(String, Int)] -> Int -> [Int] -> ([(String, Int)], Bool)
getNextInstructions instructions offset history
  | offset `elem` history = ([], True)
  | offset >= length instructions = ([], False)
  | otherwise =
    let (nextInstructions, infinite) =
          getNextInstructions instructions newOffset (offset : history)
     in (instruction : nextInstructions, infinite)
  where
    newOffset = getNewOffset instruction offset
    instruction = instructions !! offset

getFiniteInstructions :: [(String, Int)] -> Int -> [(String, Int)]
getFiniteInstructions instructions offset
  | operation == "nop" =
    let newInstructions = left ++ [("jmp", argument)] ++ tail right
     in let (newNextInstructions, infinite) = getNextInstructions newInstructions 0 []
         in if infinite
              then getFiniteInstructions instructions (offset + 1)
              else newNextInstructions
  | operation == "jmp" =
    let newInstructions = left ++ [("nop", argument)] ++ tail right
     in let (newNextInstructions, infinite) = getNextInstructions newInstructions 0 []
         in if infinite
              then getFiniteInstructions instructions (offset + 1)
              else newNextInstructions
  | offset >= length instructions = []
  | otherwise = getFiniteInstructions instructions (offset + 1)
  where
    (left, right) = splitAt offset instructions
    (operation, argument) = instructions !! offset

toArgument :: (String, Int) -> Int
toArgument (_, a) = a

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitInputs = map (splitOn " ") rawInputs -- [["com", "123"]]
  let instructions = map toInstructions splitInputs -- [("com", 123)]
  let nextInstructions = getFiniteInstructions instructions 0
  let accInstructions = take (length instructions) [(i, a) | (i, a) <- nextInstructions, i == "acc"]
  let accArguments = map toArgument accInstructions
  let total = sum accArguments
  print total
