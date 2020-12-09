import Data.List
import Data.List.Split

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  print $ length rawInputs