import Common
import Data.List
import Data.List.Split

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let timeToDepart = (toInt . head) rawInputs
  let buses = (sort . map toInt . filter (/="x") . splitOn "," . last) rawInputs
  let earliestMinutes = [ [bus - timeToDepart `mod` bus, bus] | bus <- buses]
  let sorted = sort earliestMinutes
  print $ timeToDepart
  print $ buses
  print $ earliestMinutes
  print $ sorted
  print $ (product . head) sorted