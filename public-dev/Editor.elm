module Editor where

import Dict
import Touch
import Window
import Debug
import Navigation (..)
import Canvas (..)
import Eraser (stepEraser)


-- MODEL

data Action = Undo
            | ZoomIn
            | ZoomOut
            | None
            | Touches [Touch.Touch]


data Mode = Drawing
          | Erasing
          | Viewing



type Input =
  { mode : Mode
  , action : Action
  , brush : Brush
  , canvasDims : (Int, Int)
  , windowDims : (Int, Int)
  }


getCanvas : Zoomable Canvas -> Canvas
getCanvas c = { drawing = c.drawing
              , history = c.history
              , dimensions = c.dimensions }


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

port brushPort : Signal { size : Float, color : { red : Int, green : Int, blue : Int, alpha : Float }  }
port actionPort : Signal String
port modePort : Signal String



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
               ~ brushPort
               ~ constant (10000, 7000)-- for now
               ~ Window.dimensions


withinWindowDims : [Touch.Touch] -> (Int, Int) -> [Touch.Touch]
withinWindowDims ts (w, h) =
  let within t = t.x > 0 && t.x < w && t.y > 0 && t.y < h
  in filter within ts


-- OUTPUT


getDrawing : Input -> Zoomable Canvas -> Maybe ({ absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                , drawing : [{points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
getDrawing {mode} {drawing, absPos, zoomOffset, zoom, dimensions, windowDims} =
  case mode of
    Viewing -> Just {drawing = Dict.values drawing, absPos = absPos, zoomOffset = zoomOffset, zoom = zoom, dimensions = dimensions, windowDims = windowDims}
    _ -> Nothing

port canvasOut : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                        , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
port canvasOut = getDrawing <~ input ~ canvasState


-- UPDATE

eraserBrush : Brush
eraserBrush = { size = 15, color = toRgb (rgba 0 0 0 0.1) }

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
                      Erasing -> stepEraser (applyBrush ts' eraserBrush) c
                      _       -> c
      _           -> c
    zcanvas' = withinBounds dimensions <| case action of
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
