module Common where
import Data.List (sortBy)
import Data.Function (on)

toInt :: String -> Int
toInt s = read s :: Int

toTuple :: [String] -> (String, String)
toTuple [s1, s2] = (s1, s2)

readInputByLine :: String -> IO [String]
readInputByLine path = do lines <$> readFile path

sortTupleSnd :: Ord b => [(a, b)] -> [(a, b)]
sortTupleSnd = sortBy (compare `on` snd)