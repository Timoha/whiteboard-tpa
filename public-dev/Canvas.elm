module Canvas where

import Set
import Dict
import Touch
import Window



data Action = Drawing [Brushed Touch.Touch]
            | Erasing [Brushed Touch.Touch]
            | Undo
            | ZoomIn
            | ZoomOut
            | Default
            | Touch.Touch

data Undo = Erased [Stroke] | Drawn Int


type Input = { mode : Mode, action : Action, touches : [Touch.Touch] }


type History = Dict.Dict Int Undo
type Doodle = Dict.Dict Int Stroke
type Brush = { size : Float, color : Color}
type Brushed a = { a | brush : Brush }
type Stroke = { id : Int, points : [Point], brush : Brush }
type Point = { x : Float, y : Float }
type Line = { p1 : Point, p2 : Point }
type Canvas = { drawing: Doodle, mode: Action, history: [Undo] }



port newBrush : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }
port undoAction : Signal ()
port mode : Signal String



portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }



modeToAction : String -> [Touch.Touch] -> Brush -> Action
modeToAction mode ts brush =
  case mode of
    "Drawing" -> Drawing (applyBrush ts brush)
    "Erasing" -> Erasing (applyBrush ts { size = 8, color = rgba 0 0 0 0.4})



events : Signal Action
events = merges [ modeToAction <~ mode ~ Touch.touches ~ (portToBrush <~ newBrush)
                , always Undo <~ undoAction
                ]



historyState : Signal History
historyState =
  let
    update event history =
      case event of
        Drawing ts -> addDrawn ts history
        Erasing ts -> addChange ts (Erased []) history
        _          -> history
  in
    foldp update Dict.empty events



addChange : [Brushed Touch.Touch] -> Undo -> History -> History
addChange ts v h = case ts of
  [] -> h
  _  -> Dict.insert (abs (head ts).id) v h


addDrawn : [Brushed Touch.Touch] -> History -> History
addDrawn ts history =
  foldl (\t h -> Dict.insert (abs t.id) (Drawn <| abs t.id) h) history ts


canvasState : Signal Doodle
canvasState = foldp stepCanvas Dict.empty events



stepCanvas : Action -> Canvas -> Canvas
stepCanvas event ({drawing, mode, history} as canvas) =
  case event of
    Drawing ts -> addN ts paths
    Erasing ts -> addHead ts paths
    Undo      -> undo paths



point : Float -> Float -> Point
point x y = { x = x, y = y }



pointToTuple : Point -> (Float, Float)
pointToTuple p = (p.x, p.y)



line : Point -> Point -> Line
line p1 p2 = { p1 = p1, p2 = p2 }



linesIntersection : Line -> Line -> Maybe Point
linesIntersection l1 l2 =
  let
    dx12  = l1.p1.x - l1.p2.x
    dx34  = l2.p1.x - l2.p2.x
    dy12  = l1.p1.y - l1.p2.y
    dy34  = l2.p1.y - l2.p2.y
    den = dx12 * dy34  - dy12 * dx34
  in
    if den == 0 then Nothing else
       let
         det12 = l1.p1.x * l1.p2.y - l1.p1.y * l1.p2.x
         det34 = l2.p1.x * l2.p2.y - l2.p1.y * l2.p2.x
         numx  = det12 * dx34 - dx12 * det34
         numy  = det12 * dy34 - dy12 * det34
       in Just <| point (numx / den) (numy / den)



isIntersect : Line -> Line -> Bool
isIntersect l1 l2 =
  case linesIntersection l1 l2 of
    Nothing -> False
    Just _  -> True



toSegments : [Point] -> [Line]
toSegments ps =
  let
    connectPrev p ls =
      let p1 = (head ls).p1
      in  line p1 p :: tail ls
    startNew p ls = line p p :: ls
    connect p ls = startNew p <| connectPrev p ls
  in tail <| foldl connect (line (head ps) (head ps) :: []) ps



isStrokesIntersect : Stroke -> Stroke -> Bool
isStrokesIntersect s1 s2 =
  case (length s1.points) > 1 || (length s2.points) > 1 of
    True -> let
              segs1 = toSegments s1.points
              segs2 = toSegments s2.points
            in any (\x -> any (isIntersect x) segs2) segs1
    _    -> False



isLineIntersectStroke : Line -> Stroke -> Bool
isLineIntersectStroke l s =
  case (length s.points) > 1 of
    True -> any (isIntersect l) <| toSegments s.points
    _    -> False


strokesCrossed : Stroke -> [Stroke] -> [Stroke]
strokesCrossed s ss = filter (isStrokesIntersect s) ss



undo : Doodle -> Doodle
undo d =
  let
    ids = Dict.keys d
  in
    case ids of
      [] -> Dict.empty
      _  -> Dict.remove (maximum ids) d



applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts



addN : [Brushed Touch.Touch] -> Doodle -> Doodle
addN ts dict = foldl add1 dict ts



add1 : Brushed Touch.Touch -> Doodle -> Doodle
add1 t d =
  let
    id = (abs t.id)
    vs = Dict.getOrElse {brush = t.brush, points = [], id = id} id d
  in
    Dict.insert id {vs | points <- point (toFloat t.x) (toFloat -t.y) :: vs.points} d


addHead : [Brushed Touch.Touch] -> Doodle -> Doodle
addHead ts d = case ts of
  [] -> d
  _  -> add1 (head ts) d


thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}



dot : Point -> Brush -> Form
dot pos brush = move (pointToTuple pos) <| filled brush.color (circle <| brush.size / 2)



scene : (Int,Int) -> [Stroke] -> Element
scene (w,h) paths =
  let
    float (a,b) = (toFloat a, toFloat -b)
    strokeOrDot path =
      case (length path.points) > 1 of
        True -> traced (thickLine path.brush) <| map pointToTuple path.points
        False -> dot (head path.points) path.brush
    forms = map strokeOrDot paths
  in collage w h [ move (float (-w `div` 2, -h `div` 2)) (group forms) ]



main = scene <~ Window.dimensions
              ~ (Dict.values <~ canvasState)
