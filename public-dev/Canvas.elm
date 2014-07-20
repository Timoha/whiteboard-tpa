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
                      , minZoom : Float
                      , maxZoom : Float
                      , absPos : (Float, Float)
                      , zoomOffset : (Float, Float)
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
  , dimensions = (2000, 1300)
  , windowDims = (0, 0)
  , zoom = 1
  , minZoom = 1
  , maxZoom = 2 ^ 4
  , absPos = (0, 0)
  , zoomOffset = (0, 0)
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
actions = merges [ Touches <~ (withinWindowDims <~ Touch.touches ~ Window.dimensions)
                 , portToAction <~ actionPort
                 ]


input : Signal Input
input = Input <~ (portToMode <~ modePort)
               ~ actions
               ~ (portToBrush <~ brushPort)
               ~ constant (10000, 7000)-- for now
               ~ Window.dimensions


withinWindowDims : [Touch.Touch] -> (Int, Int) -> [Touch.Touch]
withinWindowDims ts (w, h) =
  let within t = t.x > 0 && t.x < w && t.y > 0 && t.y < h
  in filter within ts


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



stepUndo : Canvas -> Canvas
stepUndo ({drawing, history} as c)  =
  let ids = Dict.keys history
  in if isEmpty ids
     then c
     else
       let
         lastId = maximum ids
         (d, h) = case Dict.get lastId history of
           Nothing          -> ( Dict.empty, Dict.empty )
           Just (Drew id)   -> ( Dict.remove id drawing, Dict.remove id history )
           Just (Erased ss) -> ( foldl (\s d -> Dict.insert s.id s d) drawing ss
                               , foldl (\s h -> Dict.insert s.id (Drew s.id) h)
                                       (Dict.remove lastId history) ss )
      in { c | drawing <- d, history <- h }



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
  in Dict.insert id {vs | points <- point t.x t.y :: vs.points} d



removeEraser : Canvas -> Canvas
removeEraser ({drawing, history} as c) =
  let
    ids = Dict.keys history
    (d, h) = if isEmpty ids
             then ( Dict.empty,  Dict.empty )
             else
               let
                 lastId = maximum ids
                 removeLast = Dict.remove lastId
               in case Dict.get lastId history of
                 Just (Erased ss) -> if isEmpty ss
                                     then ( removeLast drawing
                                          , removeLast history )
                                     else ( removeLast drawing
                                          , history )
                 _                -> ( drawing, history )
  in { c | drawing <- d, history <- h }




stepEraser : [Brushed Touch.Touch] -> Canvas -> Canvas
stepEraser ts ({drawing, history} as c) =
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
                   (Erased vs) = Dict.getOrElse (Erased []) (Debug.log "t.id" id) history
                 in {c | drawing <- foldl (\s -> Dict.remove s.id) (add1 t drawing) erased
                       , history <- Dict.insert id (Erased <| erased ++ vs) history }
      Nothing -> {c | drawing <- add1 t drawing
                    , history <- Dict.insert id (Erased []) history }




stepMove : [Touch.Touch] -> Zoomable Canvas -> Zoomable Canvas
stepMove ts ({lastMove, zoom, absPos} as c) =
  let
    (x, y) = absPos
  in if isEmpty ts
  then { c | lastMove <- Nothing }
  else
    let
      t = head ts
      float (a, b) = (toFloat a, toFloat b)
    in case lastMove of
       Just (tx, ty) ->
         let
            (dx, dy) = float ((tx - t.x), (ty - t.y))
            x' = x + dx / zoom
            y' = y + dy / zoom
         in { c | lastMove <- Just (t.x, t.y)
                , absPos  <- (x', y')}
       Nothing -> { c | lastMove <- Just (t.x, t.y) }



minScale : (Float, Float) -> (Float,Float) -> Float
minScale (winW, winH) (w,h) =
  max (winW / w) (winH / h)


withinBounds : Zoomable Canvas -> Zoomable Canvas
withinBounds ({zoom, absPos, zoomOffset, dimensions, windowDims} as c) =
  let
    scaleF f (a, b) = (a / f, b / f)
    float (a, b) = (toFloat a, toFloat b)
    addT (x, y) (dx, dy) = (x + dx, y + dy)
    subT (x, y) (dx, dy) = (x - dx, y - dy)
    limitLeftTop (x, y) (x', y') = (max x x', max y y')
    limitRightBottom (x, y) (x', y') = (min x x', min y y')
    leftTop = limitLeftTop (addT absPos zoomOffset) (0, 0)
    windowDims' = scaleF zoom <| float windowDims
    (w, h) = (float dimensions)
    (right, bottom) = limitRightBottom (addT leftTop windowDims') (w, h)
    absPos' = if right < w && bottom < h
              then subT leftTop zoomOffset
              else subT (subT (right, bottom) windowDims') zoomOffset
  in { c | absPos <- absPos' }



stepZoom : Float -> Zoomable Canvas -> Zoomable Canvas
stepZoom factor ({windowDims, zoom, minZoom, maxZoom, zoomOffset} as c) =
  let
    scaleF f (a, b) = (a / f, b / f)
    float (a, b) = (toFloat a, toFloat b)
    delta (a, b) (a', b') = (a - a', b - b')
    zoom' = min (max (zoom * factor) minZoom) maxZoom
    winD  = scaleF zoom <| float windowDims
    winD' = scaleF zoom' <| float windowDims
    (dx, dy) = scaleF 2 <| delta winD winD'
    (x, y) = zoomOffset
  in { c | zoom    <- zoom'
         , zoomOffset <- (x + dx, y + dy)}



scaleTouches : (Float, Float) -> (Float, Float) -> Float -> Touch.Touch -> Touch.Touch
scaleTouches (x, y) (dx, dy) zoom t =
  let
    float (a, b) = (toFloat a, toFloat b)
    (tx, ty) = float (t.x, t.y)
  in { t | x <- Debug.log "x" <| round (tx / zoom + x + dx)
         , y <- Debug.log "y" <| round (ty / zoom + y + dy) }



stepCanvas : Input -> Zoomable Canvas -> Zoomable Canvas
stepCanvas {mode, action, brush, canvasDims, windowDims}
           ({drawing, history, dimensions, zoom, lastMove, absPos, zoomOffset} as zcanvas'') =
  let
    float (a, b) = (toFloat a, toFloat b)
    zcanvas = { zcanvas'' | windowDims <- windowDims
                          , minZoom <- max 1 (minScale (float windowDims) (float dimensions)) }
    c = getCanvas zcanvas
    canvas' = case action of
      Undo       -> stepUndo c
      Touches ts -> let
                  ts' = map (scaleTouches absPos zoomOffset zoom) ts
                in case mode of
                  Drawing -> { c | drawing <- addN (applyBrush ts' brush) drawing
                                 , history <- recordDrew ts' history }
                  Erasing -> stepEraser (applyBrush ts' { size = 15, color = rgba 0 0 0 0.1 }) c
                  _       -> c
      _           -> c
    zcanvas' = withinBounds <| case action of
        ZoomIn  -> stepZoom 2 zcanvas
        ZoomOut -> stepZoom (1 / 2) zcanvas
        Touches ts -> case mode of
                         Viewing -> stepMove ts zcanvas
                         _ -> zcanvas
        _          -> zcanvas
  in
    { zcanvas' | drawing <- canvas'.drawing
               , history <- canvas'.history }



canvasState : Signal (Zoomable Canvas)
canvasState = foldp stepCanvas defaultCanvas input



-- VIEW


thickLine : Brush -> LineStyle
thickLine brush = { defaultLine | color <- brush.color
                                , width <- brush.size
                                , join <- Smooth
                                , cap <- Round }



dot : (Float, Float) -> Brush -> Form
dot pos brush = move pos <| filled brush.color (circle <| brush.size / 2)



display : (Int, Int) -> Zoomable Canvas -> Element
display (w, h) ({drawing, history, zoom, absPos} as canvas) =
  let
    float (a, b) = (toFloat a, toFloat b)
    flipVert (a, b) = (a, -b)
    paths = Dict.values drawing
    strokeOrDot p =
      if (length p.points) > 1
      then traced (thickLine p.brush) <| map (flipVert . pointToTuple) p.points
      else dot (flipVert . pointToTuple . head <| p.points) p.brush
    forms = map strokeOrDot paths
    toZero zoom (w, h) = (-w * zoom / 2, h * zoom / 2)
    toAbsPos (dx, dy) (x, y) = (x - dx * zoom, y + dy * zoom )
    pos = toAbsPos absPos <| toZero zoom (float (w, h))
  in collage w h [ scale zoom <| move pos (group forms) ]



main = display <~ Window.dimensions
                ~ canvasState
