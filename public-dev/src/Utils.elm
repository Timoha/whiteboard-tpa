module Utils where

flipVert : (number, number) -> (number, number)
flipVert (a, b) = (a, -b)


floatT : (Int, Int) -> (Float, Float)
floatT (a, b) = (toFloat a, toFloat b)


scaleF : Float -> (Float, Float) -> (Float, Float)
scaleF f (a, b) = (a / f, b / f)

addT (x, y) (dx, dy) = (x + dx, y + dy)
subT (x, y) (dx, dy) = (x - dx, y - dy)