module Canvas where

import Set
import Dict
import Touch
import Window



data Action = DrawStroke [Brushed Touch.Touch]
            | Undo
            | Mode

data Undo = Erase Int | Draw Stroke
data Mode = Drawing
          | Erasing
          | Moving


type Drawing = Dict.Dict Int Stroke
type Brush = { size : Float, color : Color}
type Brushed a = { a | brush : Brush }
type Stroke = {id : Int, points : [Point], brush : Brush }
type Point = { x : Float, y : Float }
type Line = { p1 : Point, p2 : Point }



port newBrush : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }
port undoAction : Signal ()
port mode : Signal String



portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }



portToMode : String -> Mode
portToMode s = case s of
    "Drawing" -> Drawing
    "Erasing" -> Erasing




events : Signal Action
events = merges [ DrawStroke <~ (applyBrush <~ Touch.touches ~ (portToBrush <~ newBrush))
                , always Undo <~ undoAction
                , portToMode <~ mode
                ]



point : Float -> Float -> Point
point x y = { x = x, y = y }


pointToTuple : Point -> (Float, Float)
pointToTuple p = (p.x, p.y)



line : Point -> Point -> Line
line p1 p2 = { p1 = p1, p2 = p2 }


lineToTuple : Line -> (Point, Point)
lineToTuple l = (l.p1, l.p2)


linesIntersection : (Point, Point) -> (Point, Point) -> Maybe Point
linesIntersection (p1, p2) (p3, p4) =
    let
        dx12  = p1.x - p2.x
        dx34  = p3.x - p4.x
        dy12  = p1.y - p2.y
        dy34  = p3.y - p4.y
        den = dx12 * dy34  - dy12 * dx34
    in if den == 0 then Nothing else
           let
               det12 = p1.x * p2.y - p1.y * p2.x
               det34 = p3.x * p4.y - p3.y * p4.x
               numx  = det12 * dx34 - dx12 * det34
               numy  = det12 * dy34 - dy12 * det34
           in  Just <| point (numx / den) (numy / den)



isIntersect : Line -> Line -> Bool
isIntersect l1 l2 =
  case linesIntersection (lineToTuple l1) (lineToTuple l2) of
      Nothing -> False
      Just _  -> True



toSegments : [Point] -> [Line]
toSegments ps =
    let connectPrev p ls = let p1 = (head ls).p1
                           in  line p1 p :: tail ls
        startNew p ls = line p p :: ls
        connect p ls = startNew p <| connectPrev p ls
    in  tail <| foldl connect (line (head ps) (head ps) :: []) ps



isStrokesIntersect : Stroke -> Stroke -> Bool
isStrokesIntersect s1 s2 =
    case (length s1.points) > 1 || (length s2.points) > 1 of
        True -> let segs1 = toSegments s1.points
                    segs2 = toSegments s2.points
                in  any (\x -> any (isIntersect x) segs2) segs1
        _    -> False



strokesCrossed : Stroke -> [Stroke] -> [Stroke]
strokesCrossed s ss = filter (isStrokesIntersect s) ss




canvasState : Signal Drawing
canvasState =
    let update event paths = case event of
                                 DrawStroke strokes -> addN strokes paths
                                 Undo               -> undo paths
    in  foldp update Dict.empty events



undo : Drawing -> Drawing
undo d = let ids = Dict.keys d
         in  case ids of
             [] -> Dict.empty
             _  -> Dict.remove (maximum ids) d



applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts



addN : [Brushed Touch.Touch] -> Drawing -> Drawing
addN ts dict = foldl add1 dict ts



add1 : Brushed Touch.Touch -> Drawing -> Drawing
add1 t d = let id = (abs t.id)
               vs = Dict.getOrElse {brush = t.brush, points = []} id d
           in  Dict.insert id {vs | points <- point (toFloat t.x) (toFloat -t.y) :: vs.points,
                                    id     <- t.id
                              } d



thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}



dot : Point -> Brush -> Form
dot pos brush = move (pointToTuple pos) <| filled brush.color (circle <| brush.size / 2)



scene : (Int,Int) -> [Stroke] -> Element
scene (w,h) paths =
    let float (a,b) = (toFloat a, toFloat -b)
        strokeOrDot path =
            case (length path.points) > 1 of
                True -> traced (thickLine path.brush) <| map pointToTuple path.points
                False -> dot (head path.points) path.brush
        forms = map strokeOrDot paths
    in  collage w h [ move (float (-w `div` 2, -h `div` 2)) (group forms) ]



main = scene <~ Window.dimensions
              ~ (Dict.values <~ canvasState)
