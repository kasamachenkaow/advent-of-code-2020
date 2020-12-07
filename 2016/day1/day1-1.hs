import Data.List
import Data.List.Split

toDirectionAndSteps :: String -> (Char, Int)
toDirectionAndSteps (direction : steps) = (direction, read steps :: Int)

toModifierAndSteps :: [(Char, Int)] -> (Int, Int) -> [([Int], Int)]
toModifierAndSteps (('L', steps) : xs) (mx, my) = ([- mx, - my], steps) : toModifierAndSteps xs (- my, mx)
toModifierAndSteps (('R', steps) : xs) (mx, my) = ([mx, my], steps) : toModifierAndSteps xs (my, - mx)
toModifierAndSteps _ _ = []

toMove :: [Int] -> [([Int], Int)] -> [([Int], Int)]
toMove lastXY ((_, 0) : xs) = toMove lastXY xs
toMove lastXY ((modifier, steps) : xs) = (nextXY, steps) : toMove nextXY ((modifier, steps -1) : xs)
  where
    nextXY = map sum (transpose [lastXY, modifier])
toMove _ _ = []

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let splitInputs = splitOn ", " (head rawInputs)
  let directionAndSteps = map toDirectionAndSteps splitInputs
  let modifierAndSteps = toModifierAndSteps directionAndSteps (1, 0)
  let allVisitedPlaces = toMove [0, 0] modifierAndSteps
  print allVisitedPlaces
  let ([x, y], _) = last allVisitedPlaces
  print $ abs x + abs y