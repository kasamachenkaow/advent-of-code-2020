occupiedCount :: Char -> Int
occupiedCount '#' = 1
occupiedCount  _  = 0

getFirstSeenSeat :: [String] -> (Int, Int) -> (Int, Int) -> Char
getFirstSeenSeat list (row, col) (dRow, dCol)
  | row == 0 || row == length list-1 = '.'
  | col == 0 || col == length (list !! row)-1 = '.'
  | nextSeat == '.' = getFirstSeenSeat list (newRow, newCol) (dRow, dCol)
  | otherwise = nextSeat
  where
    nextSeat = (list !! newRow) !! newCol
    newCol = col + dCol
    newRow = row + dRow

countOccupiedAdjacents :: [String] -> (Int, Int) -> Int
countOccupiedAdjacents list (row, col) = (sum . map occupiedCount)  [lT, mT, rT, l, r, lB, mB, rB]
  where
    lT = getFirstSeenSeat list (row, col) (-1, -1)
    mT = getFirstSeenSeat list (row, col) (-1, 0)
    rT = getFirstSeenSeat list (row, col) (-1, 1)
    l  = getFirstSeenSeat list (row, col) (0, -1)
    r  = getFirstSeenSeat list (row, col) (0, 1)
    lB = getFirstSeenSeat list (row, col) (1, -1)
    mB = getFirstSeenSeat list (row, col) (1, 0)
    rB = getFirstSeenSeat list (row, col) (1, 1)

toNewSeatStatus :: Char -> Int -> Char
toNewSeatStatus 'L' 0 = '#'
toNewSeatStatus '#' occupied
  | occupied >= 5 = 'L'
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
