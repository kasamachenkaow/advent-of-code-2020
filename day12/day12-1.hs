toActionAndValue :: String -> (Char, Int)
toActionAndValue (direction : value) = (direction, read value :: Int)

move :: [(Char, Int)] -> (Int, Int) -> Int -> (Int, Int)
move (('E', value):actions) (ew, ns) direction = move actions (ew+value, ns) direction
move (('W', value):actions) (ew, ns) direction = move actions (ew-value, ns) direction
move (('N', value):actions) (ew, ns) direction = move actions (ew, ns+value) direction
move (('S', value):actions) (ew, ns) direction = move actions (ew, ns-value) direction
move (('R', value):actions) (ew, ns) direction = move actions (ew, ns) ((direction + value) `mod` 360)
move (('L', value):actions) (ew, ns) direction = move actions (ew, ns) ((direction - value) `mod` 360)
move (('F', value):actions) (ew, ns) 90 = move actions (ew+value, ns) 90
move (('F', value):actions) (ew, ns) 270 = move actions (ew-value, ns) 270
move (('F', value):actions) (ew, ns) 0 = move actions (ew, ns+value) 0
move (('F', value):actions) (ew, ns) 180 = move actions (ew, ns-value) 180
move _ lastPos _ = lastPos

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let actions = map toActionAndValue rawInputs
  let lastPos = move actions (0,0) 90
  print $ lastPos
  print $ abs (fst lastPos) + abs (snd lastPos)