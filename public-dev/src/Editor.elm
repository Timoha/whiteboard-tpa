module Editor where

import Dict
import Touch
import Window
import Debug
import Navigation (..)
import Canvas (..)
import Eraser (stepEraser)
import History (..)


-- MODEL

data Action = Undo
            | ZoomIn
            | ZoomOut
            | None
            | Touches [Touch.Touch]
            | View
            | Draw
            | Erase


data Mode = Drawing
          | Erasing
          | Viewing


data Event = Erased [(Int, Stroke)] | Drew Int

type Whiteboard =  Zoomable (Undoable Editor)


type Input =
  { action : Action
  , brush : Brush
  , canvasDims : (Int, Int)
  , windowDims : (Int, Int)
  }



type Undoable a = { a | history : History }
type Editor = { mode : Mode, canvas : Canvas }



defaultCanvas : Canvas
defaultCanvas =
  { drawing = Dict.empty
  , dimensions = (2000, 1300)
  }


defaultEditor : Whiteboard
defaultEditor =
  { canvas = defaultCanvas
  , mode = Drawing
  , history = Dict.empty
  , zoom = 1
  , minZoom = 1
  , maxZoom = 2 ^ 4
  , zoomOffset = (0, 0)
  , absPos = (0, 0)
  , windowDims = (0, 0)
  , lastMove = Nothing
  }

-- INPUT

port brushPort : Signal { size : Float, color : { red : Int, green : Int, blue : Int, alpha : Float }  }
port actionPort : Signal String


portToAction : String -> Action
portToAction s =
  case s of
    "Draw"    -> Draw
    "View"    -> View
    "Erase"   -> Erase
    "Undo"    -> Undo
    "ZoomIn"  -> ZoomIn
    "ZoomOut" -> ZoomOut
    "None"    -> None


actions : Signal Action
actions = merges [ Touches <~ (withinWindowDims <~ Touch.touches ~ Window.dimensions)
                 , portToAction <~ actionPort
                 ]


input : Signal Input
input = Input <~ actions
               ~ brushPort
               ~ constant (2000, 1300)-- for now
               ~ Window.dimensions


withinWindowDims : [Touch.Touch] -> (Int, Int) -> [Touch.Touch]
withinWindowDims ts (w, h) =
  let within t = t.x > 0 && t.x < w && t.y > 0 && t.y < h
  in filter within ts


-- OUTPUT


getDrawing : Whiteboard -> Maybe ({ absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                  , drawing : [{points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
getDrawing {mode, canvas, absPos, zoomOffset, zoom, windowDims} =
  case mode of
    Viewing -> Just {drawing = Dict.values canvas.drawing, absPos = absPos, zoomOffset = zoomOffset, zoom = zoom, dimensions = canvas.dimensions, windowDims = windowDims}
    _ -> Nothing

port canvasOut : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                        , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
port canvasOut = getDrawing <~ editorState


-- UPDATE


getUndoable : Whiteboard -> Undoable Editor
getUndoable w = { canvas = w.canvas
                , mode = w.mode
                , history = w.history
                }

getZoomable : Whiteboard -> Zoomable Editor
getZoomable w = { canvas = w.canvas
                , mode = w.mode
                , windowDims = w.windowDims
                , zoom = w.zoom
                , minZoom = w.minZoom
                , maxZoom = w.maxZoom
                , absPos = w.absPos
                , zoomOffset = w.zoomOffset
                , lastMove = w.lastMove }





stepMode : Action -> Mode -> Mode
stepMode action mode =
  case action of
    View  -> Viewing
    Draw  -> Drawing
    Erase -> Erasing
    _     -> mode



eraserBrush : Brush
eraserBrush = { size = 15, color = toRgb (rgba 0 0 0 0.1) }

stepEditor : Input -> Whiteboard -> Whiteboard
stepEditor {action, brush, canvasDims, windowDims}
           ({mode, canvas, history, zoom, absPos, zoomOffset} as editor) =
  let
    float (a, b) = (toFloat a, toFloat b)
    editor' = { editor | windowDims <- windowDims
                       , minZoom <- max 1 (minScale (float windowDims) (float canvas.dimensions)) }
  in case action of
    Undo    -> let (drawing', history') = stepUndo canvas.drawing history
               in { editor' | canvas <- { canvas | drawing <- drawing' }, history <- history' }

    ZoomIn  -> let zoomed = stepZoom 2 <| getZoomable editor'
               in {editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset}

    ZoomOut -> let zoomed = stepZoom (1/2) <| getZoomable editor'
               in {editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset}

    View    -> { editor' | mode <- Viewing }

    Draw    -> { editor' | mode <- Drawing }

    Erase   -> { editor' | mode <- Erasing }

    Touches ts -> let ts' = map (scaleTouches absPos zoomOffset zoom) ts
                  in case mode of
                    Drawing -> { editor' | canvas <- { canvas | drawing <- addN (applyBrush ts' brush) canvas.drawing }
                                         , history <- recordDrew ts' history }
                    Erasing -> let (drawing', history') = stepEraser (applyBrush ts' eraserBrush) canvas.drawing history
                               in { editor' | canvas <- { canvas | drawing <- drawing'} , history <- history'}
                    Viewing -> let moved = withinBounds canvas.dimensions <| stepMove ts <| getZoomable editor'
                               in { editor' | absPos <- moved.absPos, lastMove <- moved.lastMove }
    None   -> editor'



editorState : Signal Whiteboard
editorState = foldp stepEditor defaultEditor input



-- VIEW



display : (Int, Int) -> Whiteboard -> Element
display (w, h) ({canvas, history, zoom, absPos} as board) =
  let
    float (a, b) = (toFloat a, toFloat b)
    strokes = Dict.values canvas.drawing
    displayCanvas = renderStrokes strokes
    toZero zoom (w, h) = (-w * zoom / 2, h * zoom / 2)
    toAbsPos (dx, dy) (x, y) = (x - dx * zoom, y + dy * zoom )
    pos = toAbsPos absPos <| toZero zoom (float (w, h))
  in collage w h [ scale zoom <| move pos displayCanvas ]



main = display <~ Window.dimensions
                ~ editorState
