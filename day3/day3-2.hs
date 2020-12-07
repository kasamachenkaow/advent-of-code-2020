import Data.List ()
import Data.List.Split ()

removeEvenItem :: [a] -> [a]
removeEvenItem (x : _ : xs) = x : removeEvenItem xs
removeEvenItem _ = []

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

countTrees :: Int -> [String] -> Int -> Int
countTrees pos (m : ms) stepRight = boolToInt (m !! pos == '#') + countTrees ((pos + stepRight) `mod` length m) ms stepRight
countTrees _ _ _ = 0

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let right1down1 = countTrees 0 rawInputs 1
  let right3down1 = countTrees 0 rawInputs 3
  let right5down1 = countTrees 0 rawInputs 5
  let right7down1 = countTrees 0 rawInputs 7
  let right1down2 = countTrees 0 (removeEvenItem rawInputs) 1
  print $ right1down1 * right3down1 * right5down1 * right7down1 * right1down2