occupiedCount :: Char -> Int
occupiedCount '#' = 1
occupiedCount  _  = 0

countOccupiedAdjacents :: [String] -> (Int, Int) -> Int
countOccupiedAdjacents list (row, col)
  | row == 0 || row == length list-1 = 0
  | col == 0 || col == length (list !! row)-1 = 0
  | otherwise = (sum . map occupiedCount)  [lT, mT, rT, l, r, lB, mB, rB]
  where
    lT = (list !! (row-1)) !! (col-1)
    mT = (list !! (row-1)) !! col
    rT = (list !! (row-1)) !! (col+1)
    l  = (list !! row) !! (col-1)
    r  = (list !! row) !! (col+1)
    lB = (list !! (row+1)) !! (col-1)
    mB = (list !! (row+1)) !! col
    rB = (list !! (row+1)) !! (col+1)

toNewSeatStatus :: Char -> Int -> Char
toNewSeatStatus 'L' 0 = '#'
toNewSeatStatus '#' occupied
  | occupied >= 4 = 'L'
toNewSeatStatus curr _ = curr

fillSeats :: [String] -> [String]
fillSeats list = [ [toNewSeatStatus ((list !! row) !! col) (coal (row, col)) | col <- [0..length (list !! row) -1]] | row <- [0..length list-1]]
  where
    coal = countOccupiedAdjacents list

findStableState :: [String] -> [String]
findStableState curr
  | new == curr = new
  | otherwise = findStableState new
  where
    new = fillSeats curr

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let stableState = findStableState rawInputs
  let allOccupiedCount = [ (sum . map occupiedCount) rowS | rowS <- stableState ]
  print $ sum allOccupiedCount
