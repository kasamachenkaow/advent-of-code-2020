toActionAndValue :: String -> (Char, Int)
toActionAndValue (direction : value) = (direction, read value :: Int)

move :: [(Char, Int)] -> (Int, Int) -> (Int, Int) -> (Int, Int)
move (('E', value):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wEW+value, wNS)
move (('W', value):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wEW-value, wNS)
move (('N', value):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wEW, wNS+value)
move (('S', value):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wEW, wNS-value)
move (('R', 90   ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wNS, -wEW)
move (('R', 180  ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (-wEW, -wNS)
move (('R', 270  ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (-wNS, wEW)
move (('L', 90   ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (-wNS, wEW)
move (('L', 180  ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (-wEW, -wNS)
move (('L', 270  ):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW, sNS) (wNS, -wEW)
move (('F', value):actions) (sEW, sNS) (wEW, wNS) = move actions (sEW+wEW*value, sNS+wNS*value) (wEW, wNS)
move _ lastPos _ = lastPos

main :: IO ()
main = do
  rawInputs <- lines <$> readFile "./input"
  let actions = map toActionAndValue rawInputs
  let lastPos = move actions (0,0) (10,1)
  print $ lastPos
  print $ abs (fst lastPos) + abs (snd lastPos)