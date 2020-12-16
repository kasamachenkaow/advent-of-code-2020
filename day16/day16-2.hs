-- O(n) mainly
import Common
import Data.List
import Data.List.Split

validate :: Int -> [Int] -> Bool
validate n rules = n >= head rules && n <= last rules

ruleStringToRules :: String -> [Int]
ruleStringToRules s = rules
  where
    rules = [ r | r <- [firstRule..lastRule], any (validate r) ranges]
    lastRule = last $ last ranges
    firstRule = head $ head ranges
    ranges = map (map toInt . splitOn "-") rawRanges
    rawRanges = splitOn " or " s

getOrderClasses :: [(String, [Int])] -> [[Int]] -> [(String, [Int])] -> [String]
getOrderClasses rules (g:gs) ((c, r):cs)
  | all (`elem` r) g = c : getOrderClasses (delete (c, r) rules) gs (delete (c, r) rules)
  | otherwise = getOrderClasses rules (g:gs) cs
getOrderClasses _ _ _ = ["suck"]

removeSingle :: [[String]] -> [String] -> [[String]]
removeSingle list (s:ss) = removeSingle reduced ss
  where
    reduced = [ if length t > 1 then delete s t else t | t <- list ]
removeSingle list _ = list

reduceToSingle :: [[String]] -> [[String]]
reduceToSingle classesList
  | any (\cs -> length cs > 1) reducedList = reduceToSingle reducedList
  | otherwise = reducedList
  where
    reducedList = removeSingle classesList [ s | (s:ss) <- classesList, null ss]

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let sections = splitOn [""] rawInputs
  let rawRules = map (splitOn ": ") $ head sections
  let rules = map (\x -> (head x, ruleStringToRules (last x))) rawRules
  let flattenRules = concatMap snd rules
  let myTicket = map toInt . splitOn "," $ last $ sections !! 1
  let nearbyTickets = map (map toInt . splitOn ",") $ tail $ sections !! 2
  let validTickets = myTicket : [ t | t <- nearbyTickets, all (`elem` flattenRules) t]
  let groupFields = transpose validTickets
  let possibleClasses = [ filter (/="") [if all (`elem` r) g then c else "" | (c, r) <- rules] | g <- groupFields]
  let columns = concat $ reduceToSingle possibleClasses
  let answers = [ myTicket !! i | i <- [0..length myTicket-1], "departure" `isPrefixOf` (columns !! i)]
  -- print rules
  print myTicket
  -- print nearbyTickets
  -- print validTickets
  -- print groupFields
  print $ length rules
  print $ length groupFields
  --  print possibleClasses
  print columns
  print answers
  print $ product answers

-- wrong answer: 1211345077559