module Common where
import Data.List.Split
import Data.List (sortBy)
import Data.Function (on)

toInt :: String -> Int
toInt s = read s :: Int

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing = 0
nothingToZero (Just _) = 1

toTuple :: [String] -> (String, String)
toTuple [s1, s2] = (s1, s2)

readInputByLine :: String -> IO [String]
readInputByLine path = do lines <$> readFile path

toListInt :: String -> [Int]
toListInt s = map toInt $ splitOn "," s

sortTupleSnd :: Ord b => [(a, b)] -> [(a, b)]
sortTupleSnd = sortBy (compare `on` snd)