module Canvas where

import Dict
import Touch
import Window


main = lift2 scene Window.dimensions
                   <| reverse . Dict.values <~ foldp addN Dict.empty
                      (applyBrush <~ Touch.touches ~ (portToBrush <~ newBrush))


type Brush = { size : Float, color: Color}
type Brushed a = { a | brush: Brush }
type Stroke = { points : [(Int, Int)], brush : Brush }

port newBrush : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }

portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }


applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts


addN : [Brushed Touch.Touch] -> Dict.Dict Int Stroke -> Dict.Dict Int Stroke
addN ts dict = foldl add1 dict ts

add1 : Brushed Touch.Touch -> Dict.Dict Int Stroke -> Dict.Dict Int Stroke
add1 t d = let vs = Dict.getOrElse {brush = t.brush, points = []} t.id d
           in  Dict.insert t.id {vs | points <- (t.x, t.y) :: vs.points} d

scene : (Int,Int) -> [Stroke] -> Element
scene (w,h) paths =
    let float (a,b) = (toFloat a, toFloat -b)
        strokeOrDot path =
            case (length path.points) > 1 of
                True -> traced (thickLine path.brush) (map float path.points)
                False -> dot (float <| head path.points) path.brush
        forms = map strokeOrDot paths
    in  collage w h [ move (float (-w `div` 2, -h `div` 2)) (group forms) ]

thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}

dot : (Float, Float) ->  Brush -> Form
dot pos brush = move pos <| filled brush.color (circle <| brush.size / 2)
