-- NOT FINISHED YET, NEED TO DEVELOP MORE
import Data.List

subListExists :: (Eq a) => [a] -> [a] -> Bool
subListExists subList (x:xs)
  | subList `isPrefixOf` (x:xs) = True
  | otherwise = subListExists subList xs
subListExists _ _ = False

main :: IO ()
main = do
  -- rawInputs <- lines <$> readFile "./input4"
  let result = [ [a,b,c,d,e,f,g,h,i,j] | a <- [1], b <- [0,2], c <- [0,3], d <- [0,4], e <- [0,5], f <- [0,6], g <- [0,7], h <- [0,8], i <- [0,9], j <- [10], (not . subListExists [0,0,0] ) [a,b,c,d,e,f,g,h,i,j]]
  let combinations = subsequences [1,2,3,4,5,6,7,8,9,10]
  let differences = [ [combination !! i - combination !! (i-1) | i <- [1..length combination-1]] | combination <- combinations]
  let result2 = [ maximum diff | diff <- differences, not $ null diff, maximum diff <= 3]
  -- print $ rawInputs
  print $ result
  print $ result2
  print $ combinations
  print $ length result
  print $ length result2

-- Input2 : 8
-- Input3 : 19208