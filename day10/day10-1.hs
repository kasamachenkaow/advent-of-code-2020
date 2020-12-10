import Data.List

toInt :: String -> Int
toInt s = read s :: Int

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let numbers = map toInt rawInputs
  let outletJoltage = 0
  let adapterJoltages = sort numbers
  let deviceJoltage = last adapterJoltages + 3
  let allJoltages = [outletJoltage] ++ adapterJoltages ++ [deviceJoltage]
  let differences = [ allJoltages !! i - allJoltages !! (i-1) | i <- [1..length allJoltages-1]]
  let sum3 = length (filter (==3) differences)
  let sum1 = length (filter (==1) differences)
  print sum3
  print sum1
  print $ sum3 * sum1