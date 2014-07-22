module Canvas where

import Dict
import Touch
-- MODEL


type Canvas =
  { drawing : Drawing
  , dimensions : (Int, Int)
  }



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


applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts



addN : [Brushed Touch.Touch] -> Drawing -> Drawing
addN ts d = foldl stepStroke d ts


stepStroke : Brushed Touch.Touch -> Drawing -> Drawing
stepStroke t d =
  let
    id = abs t.id
    vs = Dict.getOrElse {brush = t.brush, points = []} id d
  in Dict.insert id {vs | points <- point t.x t.y :: vs.points} d



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
