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
  }



type History = Dict.Dict Int Event
type Doodle = Dict.Dict Int Stroke
type Brush = { size : Float, color : Color}
type Brushed a = { a | brush : Brush }
type Stroke = { id : Int, points : [Point], brush : Brush }
type Point = { x : Float, y : Float }
type Line = { p1 : Point, p2 : Point }
type Zoomable a = { a | windowDims : (Int, Int)
                      , zoom : Float
                      , topLeft : (Int, Int)
                      , lastMove : Maybe (Int, Int) }



getCanvas : Zoomable Canvas -> Canvas
getCanvas c = { drawing = c.drawing
              , history = c.history
              , dimensions = c.dimensions }


point : Float -> Float -> Point
point x y = { x = x, y = y }



pointToTuple : Point -> (Float, Float)
pointToTuple p = (p.x, p.y)



line : Point -> Point -> Line
line p1 p2 = { p1 = p1, p2 = p2 }



defaultCanvas : Zoomable Canvas
defaultCanvas =
  { drawing = Dict.empty
  , history = Dict.empty
  , dimensions = (10000, 7000)
  , windowDims = (0, 0)
  , zoom = 1
  , topLeft = (0, 0)
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


isLineStrokeIntersect : Line -> Stroke -> Bool
isLineStrokeIntersect l s =
  if (length s.points) > 1
  then any (isIntersect l) <| toSegments s.points
  else False



undo : Canvas -> Canvas
undo ({drawing, history} as c)  =
  let ids = Dict.keys history
  in if isEmpty ids
     then c
     else
       let lastId = maximum ids
       in case Dict.get lastId history of
         Nothing          -> { c | drawing <- Dict.empty
                                 , history <- Dict.empty }
         Just (Drew id)   -> { c | drawing <- Dict.remove id drawing
                                 , history <- Dict.remove id history }
         Just (Erased ss) -> { c | drawing <- foldl (\s d -> Dict.insert s.id s d) drawing ss
                                 , history <- foldl (\s h -> Dict.insert s.id (Drew s.id) h)
                                                    (Dict.remove lastId history) ss }


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



removeEraser : Canvas -> Canvas
removeEraser ({drawing, history} as c) =
  let
    ids = Dict.keys history
  in
    case ids of
      [] -> { c | drawing <- Dict.empty
                , history <- Dict.empty }
      _  -> let lastId = maximum ids
            in case Dict.get lastId history of
              Just (Erased s) -> case s of
                                   [] -> { c | drawing <- Dict.remove lastId drawing
                                             , history <- Dict.remove lastId history }
                                   _  -> { c | drawing <- Dict.remove lastId drawing }
              _               -> c


eraser : [Brushed Touch.Touch] -> Canvas -> Canvas
eraser ts ({drawing, history} as c) =
  if isEmpty ts
  then removeEraser c
  else
    let
      t = head ts
      id = abs t.id
    in case Dict.get id drawing of
      Just s  -> let
                   eraserSeg = line (point t.x t.y) (head s.points)
                   strokes = tail . reverse <| Dict.values drawing
                   crossed = filter (isLineStrokeIntersect eraserSeg) strokes
                   erased = if isEmpty crossed then [] else [head crossed] -- erase only latest
                   (Erased vs) = Dict.getOrElse (Erased []) id history
                 in {c | drawing <- foldl (\s -> Dict.remove s.id) (add1 t drawing) erased
                       , history <- Dict.insert id (Erased <| erased ++ vs) history }
      Nothing -> {c | drawing <- add1 t drawing
                    , history <- Dict.insert id (Erased []) history }




stepMove : [Touch.Touch] -> Zoomable Canvas -> Zoomable Canvas
stepMove ts ({lastMove, dimensions, windowDims, zoom, topLeft} as c) =
  let
    (canW, canH) = dimensions
    (winW, winH) = windowDims
    (top, left) = topLeft
  in if isEmpty ts
  then { c | lastMove <- Nothing }
  else
    let
      t = head ts
      float (a, b) = (toFloat a, toFloat b)
      roundT (a, b) = (round a, round b)
    in case lastMove of
       Just (x, y) ->
         let
            (dx, dy) = float ((x - t.x), (y - t.y))
            top' = top +  (round <|  dy / zoom)
            left' = left + (round <| dx / zoom)
         in { c | lastMove <- Just (t.x, t.y)
                , topLeft  <- (top', left') }
       Nothing -> { c | lastMove <- Just (t.x, t.y) }


scaleTouches : (Int, Int) -> Float -> Touch.Touch -> Touch.Touch
scaleTouches (top, left) zoom t =
  let
    float (a, b) = (toFloat a, toFloat b)
    (x, y) = float (t.x, t.y)
  in { t | x <- round (x / zoom) + left
         , y <- round (y / zoom) + top }


--stepZoom : Float -> (Int, Int) -> Float -> ((Int, Int), Float)


stepCanvas : Input -> Zoomable Canvas -> Zoomable Canvas
stepCanvas {mode, action, brush, canvasDims, windowDims}
           ({drawing, history, dimensions, zoom, topLeft, lastMove} as zcanvas) =
  let
    c = getCanvas zcanvas
    canvas' = case action of
      Undo       -> undo c
      Touches ts -> let
                  ts' = map (scaleTouches topLeft zoom) ts
                in case mode of
                  Drawing -> { c | drawing <- addN (applyBrush ts' brush) drawing
                                 , history <- recordDrew ts' history }
                  Erasing -> eraser (applyBrush ts' { size = 15, color = rgba 0 0 0 0.1 }) c
                  _       -> c
      _           -> c
    zcanvas' = case action of
        --ZoomIn  -> stepZoom 1.5 topLeft zoom
        --ZoomOut -> stepZoom (1 / 1.5) topLeft zoom
        Touches ts' -> let
                         ts = map (scaleTouches topLeft zoom) ts'
                       in case mode of
                         Viewing -> stepMove ts' zcanvas
                         _ -> zcanvas
        _           -> zcanvas
  in
    { zcanvas' | drawing <- canvas'.drawing
               , history <- canvas'.history }



canvasState : Signal (Zoomable Canvas)
canvasState = foldp stepCanvas defaultCanvas input



-- VIEW


thickLine : Brush -> LineStyle
thickLine brush = {defaultLine | color <- brush.color,
                                 width <- brush.size, join <- Smooth, cap <- Round}



dot : (Float, Float) -> Brush -> Form
dot pos brush = move pos <| filled brush.color (circle <| brush.size / 2)



display : (Int, Int) -> Zoomable Canvas -> Element
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
  in collage w h [ move (flipVert (-w' * zoom/ 2 - left  , -h' * zoom/ 2 - top)) (scale zoom <| group forms) ]


minScale : (Float, Float) -> (Float,Float) -> Float
minScale (winW, winH) (w,h) =
  min (winW / w) (winH / h)


main = display <~ Window.dimensions
                ~ canvasState
