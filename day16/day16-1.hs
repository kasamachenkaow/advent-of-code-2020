-- O(n^2)
import Common
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

main :: IO ()
main = do
  rawInputs <- readInputByLine "./input"
  let sections = splitOn [""] rawInputs
  let rawRules = map (splitOn ": ") $ head sections
  let rules = map (\x -> (head x, ruleStringToRules (last x))) rawRules
  let myTicket = map toInt . splitOn "," $ last $ sections !! 1
  let nearbyTickets = map (map toInt . splitOn ",") $ tail $ sections !! 2
  let flattenRules = concatMap snd rules
  let flattenNearbyTickets = concat nearbyTickets
  let invalidNumbers = [ n | n <- flattenNearbyTickets, n `notElem` flattenRules]
  print rules
  print myTicket
  print nearbyTickets
  print flattenRules
  print flattenNearbyTickets
  print invalidNumbers
  print $ sum invalidNumbers