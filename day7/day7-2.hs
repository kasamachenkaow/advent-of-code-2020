import Data.List ()
import Data.List.Split

-- from "dim coral bags" to "dim coral"
-- from "dim coral bag" to "dim coral"
removeBagTerm :: String -> String
removeBagTerm = unwords . init . words

-- from ["1 mirrored green bag", "2 shiny maroon bags"]
-- to ["mirrored green bag", "shiny maroon bags", "shiny maroon bags"]
-- from ["no other bags"]
-- to []
toChildren :: [String] -> [String]
toChildren ["no other bags"] = []
toChildren (x : xs) = replicate (read (take 1 x) :: Int) (drop 2 x) ++ toChildren xs
toChildren _ = []

sanitize :: String -> (String, [String])
sanitize raw = (parent, children)
  where
    parent = (removeBagTerm . head) splitRaw
    children = map removeBagTerm (toChildren countAndChildrenTexts)
    countAndChildrenTexts = splitOn ", " (last splitRaw)
    splitRaw = splitOn " contain " (init raw)

findAllChildren :: [(String, [String])] -> [String] -> [String]
findAllChildren family (searchParent : xs) = children ++ findAllChildren family xs ++ findAllChildren family children
  where
    children = head [children | (parent, children) <- family, parent == searchParent]
findAllChildren _ _ = []

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  -- sanitize to be (parent, [children])
  let sanitizedInputs = map sanitize rawInputs
  -- find all the children which could have `shiny gold` as a parent/ancestor recursively down
  let allChildrens = findAllChildren sanitizedInputs ["shiny gold"]
  print allChildrens
  -- count
  let count = length allChildrens
  print count