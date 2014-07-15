module Canvas where

import Set
import Dict
import Touch
import Window


main = scene <~ Window.dimensions
              ~ (Dict.values <~ canvasState)


data Action = DrawStroke [Brushed Touch.Touch]
            | Undo


events : Signal Action
events = merge (DrawStroke <~ (applyBrush <~ Touch.touches ~ (portToBrush <~ newBrush)))
               (always Undo <~ undoAction)


canvasState : Signal Drawing
canvasState =
    let update event paths = case event of
                                DrawStroke strokes -> addN strokes paths
                                Undo -> undo paths
    in foldp update Dict.empty events

type Drawing = Dict.Dict Int Stroke
type Brush = { size : Float, color: Color}
type Brushed a = { a | brush: Brush }
type Stroke = { points : [(Int, Int)], brush : Brush }

port newBrush : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }
port undoAction : Signal ()



data Undo = Erase Int | Draw Stroke


undo : Drawing -> Drawing
undo d = let ids = Dict.keys d
         in case ids of
                 [] -> Dict.empty
                 _  -> Dict.remove (maximum ids) d


portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }


applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts


addN : [Brushed Touch.Touch] -> Drawing -> Drawing
addN ts dict = foldl add1 dict ts

add1 : Brushed Touch.Touch -> Drawing -> Drawing
add1 t d = let id = (abs t.id)
               vs = Dict.getOrElse {brush = t.brush, points = []} id d
           in  Dict.insert id {vs | points <- (t.x, t.y) :: vs.points} d

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

dot : (Float, Float) -> Brush -> Form
dot pos brush = move pos <| filled brush.color (circle <| brush.size / 2)
