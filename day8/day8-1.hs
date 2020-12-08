import Data.List ()
import Data.List.Split

toCommands :: [String] -> (String, Int)
toCommands [c, '+' : xs] = (c, read xs :: Int)
toCommands [c, a] = (c, read a :: Int)

getNewOffset :: (String, Int) -> Int -> Int
getNewOffset ("jmp", a) offset = offset + a
getNewOffset _ offset = offset + 1

getNextCommands :: [(String, Int)] -> Int -> [Int] -> [(String, Int)]
getNextCommands commands offset history =
  if (offset `notElem` history) && (offset < length commands)
    then command : getNextCommands commands newOffset (offset : history)
    else []
  where
    newOffset = getNewOffset command offset
    command = commands !! offset

toArgument :: (String, Int) -> Int
toArgument (_, a) = a

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitInputs = map (splitOn " ") rawInputs -- [["com", "123"]]
  let commands = map toCommands splitInputs -- [("com", 123)]
  let nextCommands = getNextCommands commands 0 []
  let accCommands = [(i, a) | (i, a) <- nextCommands, i == "acc"]
  print accCommands
  let accArguments = map toArgument accCommands
  let total = sum accArguments
  print total