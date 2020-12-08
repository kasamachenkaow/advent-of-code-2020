import Data.List ()
import Data.List.Split

toCommands :: [String] -> (String, Int)
toCommands [c, '+' : xs] = (c, read xs :: Int)
toCommands [c, a] = (c, read a :: Int)

getNewOffset :: (String, Int) -> Int -> Int
getNewOffset ("jmp", a) offset = offset + a
getNewOffset _ offset = offset + 1

getNextCommands :: [(String, Int)] -> Int -> [Int] -> ([(String, Int)], Bool)
getNextCommands commands offset history
  | offset `elem` history = ([], True)
  | offset >= length commands = ([], False)
  | otherwise =
    let (nextCommands, infinite) =
          getNextCommands commands newOffset (offset : history)
     in (command : nextCommands, infinite)
  where
    newOffset = getNewOffset command offset
    command = commands !! offset

getFiniteCommands :: [(String, Int)] -> Int -> [(String, Int)]
getFiniteCommands commands offset
  | instruction == "nop" =
    let newCommands = left ++ [("jmp", argument)] ++ tail right
     in let (newNextCommands, infinite) = getNextCommands newCommands 0 []
         in if infinite
              then getFiniteCommands commands (offset + 1)
              else newNextCommands
  | instruction == "jmp" =
    let newCommands = left ++ [("nop", argument)] ++ tail right
     in let (newNextCommands, infinite) = getNextCommands newCommands 0 []
         in if infinite
              then getFiniteCommands commands (offset + 1)
              else newNextCommands
  | offset >= length commands = []
  | otherwise = getFiniteCommands commands (offset + 1)
  where
    (left, right) = splitAt offset commands
    (instruction, argument) = commands !! offset

toArgument :: (String, Int) -> Int
toArgument (_, a) = a

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitInputs = map (splitOn " ") rawInputs -- [["com", "123"]]
  let commands = map toCommands splitInputs -- [("com", 123)]
  let nextCommands = getFiniteCommands commands 0
  let accCommands = take (length commands) [(i, a) | (i, a) <- nextCommands, i == "acc"]
  let accArguments = map toArgument accCommands
  let total = sum accArguments
  print total