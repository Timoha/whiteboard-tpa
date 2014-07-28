module Canvas where

import Dict
import Touch
import Debug
-- MODEL


type Canvas =
  { drawing : Drawing
  , dimensions : (Int, Int)
  }


type WithId a = { a | id : Int }
type Drawing = Dict.Dict Int Stroke
type Brush = { size : Float, color : { red : Int, green : Int, blue : Int, alpha : Float }}
type Brushed a = { a | brush : Brush }
type Stroke = { points : [Point], brush : Brush }
type Point = { x : Int, y : Int }
type Line = { p1 : Point, p2 : Point }


toRgbaColor :  { red : Int, green : Int, blue : Int, alpha : Float } -> Color
toRgbaColor { red, green, blue, alpha } = rgba red green blue alpha

point : Int -> Int -> Point
point x y = { x = x, y = y }



pointToTuple : Point -> (Float, Float)
pointToTuple p = (toFloat p.x, toFloat p.y)



line : Point -> Point -> Line
line p1 p2 = { p1 = p1, p2 = p2 }



-- UPDATE

touchToPoint : Touch.Touch -> WithId Point
touchToPoint t = { id = abs t.id, x = t.x, y = t.y }

applyBrush : [WithId Point] -> Brush -> [Brushed (WithId Point)]
applyBrush ps b = map (\p -> {p | brush = b}) ps



addN : [Brushed (WithId Point)] -> Drawing -> Drawing
addN ps d = foldl stepStroke d ps



stepStroke : Brushed (WithId Point) -> Drawing -> Drawing
stepStroke p d =
  let
    vs = Dict.getOrElse {brush = p.brush, points = []} p.id d
  in Dict.insert p.id {vs | points <- (point p.x p.y) :: vs.points} d



-- VIEW




thickLine : Brush -> LineStyle
thickLine brush = { defaultLine | color <- toRgbaColor brush.color
                                , width <- brush.size
                                , join <- Smooth
                                , cap <- Round }



dot : (Float, Float) -> Brush -> Form
dot pos brush = move pos <| filled (toRgbaColor brush.color) (circle <| brush.size / 2)



renderStrokes : [Stroke] -> Form
renderStrokes ss =
  let
    flipVert (a, b) = (a, -b)
    strokeOrDot s =
      if (length s.points) > 1
      then traced (thickLine s.brush) <| map (flipVert . pointToTuple) s.points
      else dot (flipVert . pointToTuple . head <| s.points) s.brush
    ss' = map strokeOrDot ss
 in group ss'
