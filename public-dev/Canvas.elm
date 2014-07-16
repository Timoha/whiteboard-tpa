module Canvas where

import Set
import Dict
import Touch
import Window

-- MODEL

data Action = Undo
            | ZoomIn
            | ZoomOut
            | Touches [Touch.Touch]


data Mode = Drawing
          | Erasing
          | Viewing

data Event = Erased [Stroke] | Drew Int


type Input =
  { mode : Mode
  , action : Action
  , brush : Brush
  , canvasDims : (Int, Int)
  }


type Canvas =
  { drawing : Doodle
  , history : History
  , dimensions : (Int, Int)
  , scale : Float
  , topLeft : (Int, Int)
  }



type History = Dict.Dict Int Event
type Doodle = Dict.Dict Int Stroke
type Brush = { size : Float, color : Color}
type Brushed a = { a | brush : Brush }
type Stroke = { id : Int, points : [Point], brush : Brush }
type Point = { x : Float, y : Float }
type Line = { p1 : Point, p2 : Point }



point : Float -> Float -> Point
point x y = { x = x, y = y }



pointToTuple : Point -> (Float, Float)
pointToTuple p = (p.x, p.y)



line : Point -> Point -> Line
line p1 p2 = { p1 = p1, p2 = p2 }



defaultCanvas : Canvas
defaultCanvas =
  { drawing = Dict.empty
  , history = Dict.empty
  , dimensions = (0, 0)
  , scale = 1
  , topLeft = (0, 0)
  }

-- INPUT

port brushPort : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }
port undoPort : Signal ()
port modePort : Signal String



portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }



portToMode : String -> Mode
portToMode s =
  case s of
    "Drawing" -> Drawing
    "Erasing" -> Erasing



actions : Signal Action
actions = merges [ Touches <~ Touch.touches
                 , always Undo <~ undoPort
                 ]


input : Signal Input
input = Input <~ (portToMode <~ modePort)
               ~ actions
               ~ (portToBrush <~ brushPort)
               ~ Window.dimensions -- for now


-- UPDATE



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



isLineStrokeIntersect : Line -> Stroke -> Bool
isLineStrokeIntersect l s =
  case (length s.points) > 1 of
    True -> any (isIntersect l) <| toSegments s.points
    _    -> False



strokesCrossed : Stroke -> [Stroke] -> [Stroke]
strokesCrossed s ss = filter (isStrokesIntersect s) ss



undo : (Doodle, History) -> (Doodle, History)
undo (d, h) =
  let
    ids = Dict.keys h
  in
    case ids of
      [] -> (d, Dict.empty)
      _  -> case Dict.get (maximum ids) h of
              Nothing               -> (Dict.empty, Dict.empty)
              Just (Drew id)        -> (Dict.remove id d, Dict.remove id h)
              Just (Erased strokes) -> (foldl (\s d -> Dict.insert s.id s d) d strokes, Dict.remove (maximum ids) h)



applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts



addN : [Brushed Touch.Touch] -> Doodle -> History -> (Doodle, History)
addN ts d h = (foldl add1 d ts, h)



add1 : Brushed Touch.Touch -> (Doodle, History) -> (Doodle, History)
add1 t (d, h) =
  let
    id = (abs t.id)
    vs = Dict.getOrElse {brush = t.brush, points = [], id = id} id d
  in
    ( Dict.insert id {vs | points <- point (toFloat t.x) (toFloat -t.y) :: vs.points} d
    , Dict.insert id (Drew id) h)



removeEraser : Doodle -> History -> (Doodle, History)
removeEraser d h =
  let
    ids = Dict.keys h
  in
    case ids of
      [] -> (d, Dict.empty)
      _  -> case Dict.get (maximum ids) h of
              Just (Erased strokes) -> (Dict.remove (maximum ids) d, h)
              _                     -> (d, h)


eraser : [Brushed Touch.Touch] -> Doodle -> History -> (Doodle, History)
eraser ts d h = case ts of
  [] -> removeEraser d h
  _  -> let
          firstT = head ts
          id = abs firstT.id
          (d', h') = case Dict.get id d of
            Just stroke -> let
                             eraserSeg = line (point firstT.x firstT.y) (head stroke.points)
                             crossed = filter (isLineStrokeIntersect eraserSeg) (Dict.values d)
                             vs = crossed
                           in (foldl (\s -> Dict.remove s.id) d crossed, Dict.insert id (Erased <| crossed ++ vs) h)
            Nothing     ->
        in (d', h')



stepCanvas : Input -> Canvas -> Canvas
stepCanvas {mode, action, brush, canvasDims}
           ({drawing, history, dimensions, scale, topLeft} as canvas) =
  let
    (drawing', history') = case action of
      Undo       -> undo (drawing, history)
      Touches ts -> case mode of
                Drawing -> addN (applyBrush ts brush) drawing history
                Erasing -> eraser (applyBrush ts { size = 15, color = rgba 0 0 0 0.5 }) drawing history
                _       -> (drawing, history)
  in
    { canvas | drawing <- drawing'
             , history <- history' }



canvasState : Signal Canvas
canvasState = foldp stepCanvas defaultCanvas input



-- VIEW


thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}



dot : Point -> Brush -> Form
dot pos brush = move (pointToTuple pos) <| filled brush.color (circle <| brush.size / 2)



display : (Int,Int) -> [Stroke] -> Element
display (w,h) paths =
  let
    float (a,b) = (toFloat a, toFloat -b)
    strokeOrDot path =
      case (length path.points) > 1 of
        True -> traced (thickLine path.brush) <| map pointToTuple path.points
        False -> dot (head path.points) path.brush
    forms = map strokeOrDot paths
  in collage w h [ move (float (-w `div` 2, -h `div` 2)) (group forms) ]



main = display <~ Window.dimensions
                ~ (Dict.values . .drawing  <~ canvasState)
