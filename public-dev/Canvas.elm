module Canvas where

import Set
import Dict
import Touch
import Window
import Debug

-- MODEL

data Action = Undo
            | ZoomIn
            | ZoomOut
            | None
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
  , windowDims : (Int, Int)
  }


type Canvas =
  { drawing : Doodle
  , history : History
  , dimensions : (Int, Int)
  , zoom : Float
  , topLeft : (Int, Int)
  , lastMove : Maybe (Int, Int)
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
  , dimensions = (10000, 7000)
  , zoom = 1
  , topLeft = (500, 500)
  , lastMove = Nothing
  }

-- INPUT

port brushPort : Signal { size : Float, red : Int, green : Int, blue : Int, alpha : Float }
port actionPort : Signal String
port modePort : Signal String



portToBrush : { size : Float, red : Int, green : Int, blue : Int, alpha : Float } -> Brush
portToBrush p = { size = p.size, color = rgba p.red p.green p.blue p.alpha }



portToMode : String -> Mode
portToMode s =
  case s of
    "Drawing" -> Drawing
    "Erasing" -> Erasing
    "Viewing" -> Viewing


portToAction : String -> Action
portToAction s =
  case s of
    "Undo"    -> Undo
    "ZoomIn"  -> ZoomIn
    "ZoomOut" -> ZoomOut
    "None"    -> None


actions : Signal Action
actions = merges [ Touches <~ Touch.touches
                 , portToAction <~ actionPort
                 ]


input : Signal Input
input = Input <~ (portToMode <~ modePort)
               ~ actions
               ~ (portToBrush <~ brushPort)
               ~ Window.dimensions -- for now
               ~ Window.dimensions -- for now



-- UPDATE


ccw : Point -> Point -> Point -> Bool
ccw a b c = (c.y - a.y) * (b.x-a.x) > (b.y - a.y) * (c.x - a.x)

{- Thanks to http://stackoverflow.com/a/9997374 -}

isIntersect : Line -> Line -> Bool
isIntersect l1 l2 = not <|
  (ccw l1.p1 l2.p1 l2.p2) == (ccw l1.p2 l2.p1 l2.p2) ||
  (ccw l1.p1 l1.p2 l2.p1) == (ccw l1.p1 l1.p2 l2.p2)


toSegments : [Point] -> [Line]
toSegments ps =
  let
    connectPrev p2 ps = case ps of
      [] -> startNew p2 []
      ps -> (line (head ps).p1 p2) :: tail ps
    startNew p1 ls = (line p1 p1) :: ls
    connect p ls = startNew p <| connectPrev p ls
  in tail <| foldl connect [] ps



isStrokesIntersect : Stroke -> Stroke -> Bool
isStrokesIntersect s1 s2 =
  if (length s1.points) > 1 || (length s2.points) > 1
  then let
         segs1 = toSegments s1.points
         segs2 = toSegments s2.points
       in any (\x -> any (isIntersect x) segs2) segs1
  else False



isLineStrokeIntersect : Line -> Stroke -> Bool
isLineStrokeIntersect l s =
  if (length s.points) > 1
  then any (isIntersect l) <| toSegments s.points
  else False



strokesCrossed : Stroke -> [Stroke] -> [Stroke]
strokesCrossed s ss = filter (isStrokesIntersect s) ss



undo : Doodle -> History -> (Doodle, History)
undo d h =
  let
    ids = Dict.keys h
  in
    case ids of
      [] -> (d, h)
      _  -> let lastId = maximum ids
            in case Dict.get lastId h of
              Nothing               -> (Dict.empty, Dict.empty)
              Just (Drew id)        -> (Dict.remove id d, Dict.remove id h)--(d, h) --
              Just (Erased ss) -> ( foldl (\s d -> Dict.insert s.id s d) d ss
                                  , foldl (\s h' -> Dict.insert s.id (Drew s.id) h') (Dict.remove lastId h) ss)



applyBrush : [Touch.Touch] -> Brush -> [Brushed Touch.Touch]
applyBrush ts b = map (\t -> {t | brush = b}) ts



recordDrew : [Touch.Touch] -> History -> History
recordDrew ts h = foldl (\t -> Dict.insert (abs t.id) (Drew <| abs t.id)) h ts



addN : [Brushed Touch.Touch] -> Doodle -> Doodle
addN ts d = foldl add1 d ts


add1 : Brushed Touch.Touch -> Doodle -> Doodle
add1 t d =
  let
    id = abs t.id
    vs = Dict.getOrElse {brush = t.brush, points = [], id = id} id d
  in
    Dict.insert id {vs | points <- point t.x t.y :: vs.points} d



removeEraser : Doodle -> History -> (Doodle, History)
removeEraser d h =
  let
    ids = Dict.keys h
  in
    case ids of
      [] -> (d, Dict.empty)
      _  -> let lastId = maximum ids
            in case Dict.get lastId h of
                    Just (Erased s) -> case s of
                                         [] -> (Dict.remove lastId d, Dict.remove lastId h)
                                         _  -> (Dict.remove lastId d, h)
                    _               -> (d, h)


eraser : [Brushed Touch.Touch] -> Doodle -> History -> (Doodle, History)
eraser ts d h = case ts of
  [] -> removeEraser d h
  _  -> let
          t = head ts
          id = abs t.id
        in case Dict.get id d of
          Nothing -> (add1 t d, Dict.insert id (Erased []) h)
          Just s  -> let
                       eraserSeg = line (point t.x t.y) (head s.points)
                       strokes = tail . reverse <| Dict.values d
                       crossed = filter (isLineStrokeIntersect eraserSeg) strokes
                       erased = if isEmpty crossed then [] else [head crossed] -- erase only latest
                       (Erased vs) = Dict.getOrElse (Erased []) id h
                     in ( foldl (\s -> Dict.remove s.id) (add1 t d) erased
                        , Dict.insert id (Erased <| erased ++ vs) h)



stepScale : Float -> Float -> Point -> (Float, Point)
stepScale factor s topLeft = (s, topLeft)



stepMove : [Touch.Touch] -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int) -> (Int, Int) -> (Maybe (Int, Int), (Int, Int))
stepMove ts (canW, canH) (winW, winH) lastMove (top, left) = case ts of
  [] -> (Nothing, (top, left))
  _  -> let
    t = head ts
  in case lastMove of
     Just (x, y) ->
       let
          top' = top + (y - t.y)
          left' = left + (x - t.x)
       in (Just (t.x, t.y), (top', left'))
     Nothing -> (Just (t.x, t.y), (top, left))


scaleTouches : (Int, Int) -> Float -> Touch.Touch -> Touch.Touch
scaleTouches (top, left) zoom t =
  { t | x <- round ((toFloat (left + t.x)) / zoom)
      , y <- round ((toFloat (top + t.y)) / zoom) }


stepCanvas : Input -> Canvas -> Canvas
stepCanvas {mode, action, brush, canvasDims, windowDims}
           ({drawing, history, dimensions, zoom, topLeft, lastMove} as canvas) =
  let
    (drawing', history') = case action of
      Undo       -> undo drawing history
      Touches ts' -> let
                  ts = map (scaleTouches topLeft zoom) ts'
                in case mode of
                Drawing -> (addN (applyBrush ts brush) drawing, recordDrew ts history)
                Erasing -> eraser (applyBrush ts { size = 15, color = rgba 0 0 0 0.1 }) drawing history
                _       -> (drawing, history)
      _          -> (drawing, history)
    (lastMove', topLeft') = case action of
     -- ZoomIn  -> stepScale 1.1 zoom topLeft
      --ZoomOut -> stepScale 0.9 zoom topLeft
        Touches ts' -> case mode of
                         Viewing -> stepMove ts' canvasDims windowDims lastMove topLeft
                         _ -> (lastMove, topLeft)
        _       -> (lastMove, topLeft)
  in
    { canvas | drawing <- drawing'
             , history <- history'
             , topLeft <- topLeft'
             , lastMove <- lastMove'
             }



canvasState : Signal Canvas
canvasState = foldp stepCanvas defaultCanvas input



-- VIEW


thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}



dot : (Float, Float) -> Brush -> Form
dot pos brush = move pos <| filled brush.color (circle <| brush.size / 2)



display : (Int, Int) -> Canvas -> Element
display (w, h) ({drawing, history, dimensions, zoom, topLeft} as canvas) =
  let
    paths = Dict.values drawing
    float (a, b) = (toFloat a, toFloat b)
    (top, left) = float topLeft
    (w', h') = float (w, h)
    flipVert (a, b) = (a, -b)
    strokeOrDot p =
      if (length p.points) > 1
      then traced (thickLine p.brush) <| map (flipVert . pointToTuple) p.points
      else dot (flipVert . pointToTuple . head <| p.points) p.brush
    forms = map strokeOrDot paths
  in collage w h [ move (flipVert ((-w' * zoom / 2) - left, (-h' * zoom / 2) - top)) (scale zoom <| group forms) ]


minScale : (Float, Float) -> (Float,Float) -> Float
minScale (winW, winH) (w,h) =
  min (winW / w) (winH / h)


main = display <~ Window.dimensions
                ~ canvasState
