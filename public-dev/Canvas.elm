module Canvas where

import Dict
import Touch
import Window


type Brush = { size : Float, r : Int, g : Int, b : Int, a : Float }
type Brushed a = { a | brush: Brush }
type Stroke = { points : [(Int, Int)], brush : Brush }

port newBrush : Signal { size : Float, r : Int, g : Int, b : Int, a : Float }

main = lift2 scene Window.dimensions
                   (reverse . Dict.values <~ foldp addN Dict.empty
                    (applyBrush <~ Touch.touches ~ newBrush))


applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts


addN : [Brushed Touch.Touch] -> Dict.Dict Int Stroke -> Dict.Dict Int Stroke
addN ts dict = foldl add1 dict ts

add1 : Brushed Touch.Touch -> Dict.Dict Int Stroke -> Dict.Dict Int Stroke
add1 t d = let vs = Dict.getOrElse {brush = t.brush, points = []} t.id d
           in  Dict.insert t.id {vs | points <- ((t.x, t.y) :: vs.points)} d

scene : (Int,Int) -> [Stroke] -> Element
scene (w,h) strokes =
    let float (a,b) = (toFloat a, toFloat -b)
        forms = group (map
                       (\stroke -> case (length stroke.points) > 1 of
                          True -> traced (thickLine stroke.brush) (map float stroke.points)
                          False -> thickDot (float (head stroke.points)) stroke.brush
                       )
                       strokes)

        picture = collage w h [ move (float (-w `div` 2, -h `div` 2)) forms ]
    in  picture

thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- rgba brush.r brush.g brush.b brush.a,
                                 width <- brush.size, join <- Smooth, cap <- Round}
thickDot : (Float, Float) ->  Brush -> Form
thickDot (x, y) brush = move (x, y) <| filled (rgba brush.r brush.g brush.b brush.a) (circle (brush.size / 2))
