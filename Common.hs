module Common where

toInt :: String -> Int
toInt s = read s :: Int

readInputByLine :: String -> IO [String]
readInputByLine path = do
    rawInputs <- lines <$> readFile path
    return rawInputs