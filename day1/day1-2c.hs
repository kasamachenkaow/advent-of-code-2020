import Common

main :: IO ()
main = do
  inputs <- lines <$> readFile "./input"
  let list = map toInt inputs
  let result = head [ (product . map (list !!)) [x, y, z] | x <- [0..length list-1], y <- [0..x-1], z <- [0..y-1], (sum . map (list !!)) [x, y, z] == 2020]
  print result