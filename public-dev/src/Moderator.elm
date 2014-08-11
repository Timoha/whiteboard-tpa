module Moderator where

import Dict
import Set
import Touch
import Window
import Debug
import Navigation (..)
import Canvas (..)
import Eraser (stepEraser, stepModerating)
import History (..)
import Utils (..)
import RealtimeApi (DrawingInfo)


import Graphics.Input (..)
import Graphics.Input as Input

data Action = Undo
            | ZoomIn
            | ZoomOut
            | NoOp
            | Touches [Touch.Touch]
            | View
            | Draw
            | Erase
            | Moderate


data Mode = Drawing
          | Erasing
          | Viewing
          | Moderating

type Input =
  { action     : Action
  , canvasDims : (Int, Int)
  , windowDims : (Int, Int)
  , drawings   : [DrawingInfo]
  }


type Moderator =  Zoomable (Undoable (Moderatable Editor))

type Editor = { mode : Mode, canvas : Canvas }

type Moderatable a = { a | drawings : Maybe [DrawingInfo] }


defaultCanvas : Canvas
defaultCanvas =
  { drawing = Dict.empty
  , dimensions = (0, 0)
  }


defaultEditor : Moderator
defaultEditor =
  { canvas = defaultCanvas
  , drawings = Nothing
  , mode = Viewing
  , history = Dict.empty
  , zoom = 1
  , minZoom = 1
  , maxZoom = 2 ^ 4
  , zoomOffset = (0, 0)
  , absPos = (0, 0)
  , windowDims = (0, 0)
  , lastPosition = Nothing
  }


port actionPort : Signal String
port canvasSizePort : Signal { width : Int, height : Int }
port submittedDrawingsPort : Signal [{drawingId:Int, firstName:String, lastName:String, strokes:Maybe [{id: Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}]

canvasSizePortToTuple : { width : Int, height : Int } -> (Int, Int)
canvasSizePortToTuple {width, height} = (width, height)

portToAction : String -> Action
portToAction s =
  case s of
    "Draw"     -> Draw
    "View"     -> View
    "Erase"    -> Erase
    "Undo"     -> Undo
    "Moderate" -> Moderate
    "ZoomIn"   -> ZoomIn
    "ZoomOut"  -> ZoomOut
    "NoOp"     -> NoOp


withinWindowDims : [Touch.Touch] -> (Int, Int) -> [Touch.Touch]
withinWindowDims ts (w, h) =
  let within t = t.x > 0 && t.x < w && t.y > 0 && t.y < h
  in filter within ts


actions : Signal Action
actions = merges [ Touches <~ (withinWindowDims <~ (Touch.touches) ~ Window.dimensions)
                 , portToAction <~ actionPort

                 --, toolActions.signal
                 ]


input : Signal Input
input = Input <~ actions
               ~ dropRepeats (canvasSizePortToTuple <~ canvasSizePort)
               ~ dropRepeats Window.dimensions
               ~ dropRepeats submittedDrawingsPort


getDrawing : Moderator -> Maybe ({ absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                  , drawing : [{id:Int, t0:Float, points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
getDrawing {mode, canvas, absPos, zoomOffset, zoom, windowDims, drawings} =
  let withOtherStrokes = sortBy .t0 <| foldl (\d ss -> maybe [] (\ss' -> ss' ++ ss) d.strokes) [] <| maybe [] id drawings
  in case mode of
    Viewing -> Just {drawing = withOtherStrokes, absPos = absPos, zoomOffset = zoomOffset, zoom = zoom, dimensions = canvas.dimensions, windowDims = windowDims}
    _ -> Nothing

port canvasOut : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                        , drawing : [{id:Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
port canvasOut = getDrawing <~ moderatorState

port erasedDrawingIdsOut : Signal [Int]
port erasedDrawingIdsOut = dropRepeats (getErased <~ submittedDrawingsPort ~ ((\m -> maybe [] id m.drawings) <~ moderatorState))


getErased : [DrawingInfo] -> [DrawingInfo] -> [Int]
getErased orig current =
  let
    origIds = map .drawingId orig |> Set.fromList
    currIds = map .drawingId current |> Set.fromList
  in Set.diff origIds currIds |> Set.toList


getZoomable : Moderator -> Zoomable Editor
getZoomable w = { canvas = w.canvas
                , mode = w.mode
                , windowDims = w.windowDims
                , zoom = w.zoom
                , minZoom = w.minZoom
                , maxZoom = w.maxZoom
                , absPos = w.absPos
                , zoomOffset = w.zoomOffset
                , lastPosition = w.lastPosition }


eraserBrush : Brush
eraserBrush = { size = 15, color = toRgb (rgba 0 0 0 0.1) }



stepUndoMod : Drawing -> [DrawingInfo] -> History -> (Drawing, [DrawingInfo], History)
stepUndoMod drawing submitted history  =
  let ids = Dict.keys history
  in if isEmpty ids
     then (drawing, submitted, history)
     else
       let
         lastId = maximum ids
       in case Dict.get lastId history of
           Nothing          -> ( Dict.empty, submitted, Dict.empty )
           Just (Drew id)   -> ( Dict.remove id drawing, submitted, Dict.remove id history )
           Just (ErasedDrawings ds)
               -> ( drawing
                  , submitted ++ ds
                  , Dict.remove lastId history )


stepModerator : Input -> Moderator -> Moderator
stepModerator {action, canvasDims, windowDims, drawings}
           ({mode, canvas, history, zoom, absPos, zoomOffset} as editor) =
  let
    editor' = { editor | windowDims <- windowDims
                       , minZoom <- max 1 <| minScale (floatT windowDims) (floatT canvas.dimensions)
                       , canvas <- { canvas | dimensions <- canvasDims }
                       , drawings <- if isNothing editor.drawings then Just drawings else editor.drawings
                       }
  in case action of
    View ->
      { editor' | mode <- Viewing }

    Moderate ->
      { editor' | mode <- Moderating }

    ZoomIn ->
      let zoomed = withinBounds editor'.canvas.dimensions <| stepZoom 2 <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset, absPos <- zoomed.absPos }

    ZoomOut ->
      let zoomed = withinBounds editor'.canvas.dimensions <| stepZoom (1/2) <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset, absPos <- zoomed.absPos }

    Undo ->
      let (drawing', drawings', history') = stepUndoMod canvas.drawing (maybe [] id editor'.drawings) history
      in { editor' | canvas <- { canvas | drawing <- drawing' }, history <- history', drawings <- Just drawings' }

    Touches ts ->
      let ps = map (touchToPoint . (scaleTouch absPos zoomOffset zoom)) ts
      in case mode of

        Moderating ->
          let (drawing', drawings', history') = stepModerating (applyBrush ps eraserBrush) canvas.drawing (maybe [] id editor'.drawings) history
          in { editor' | canvas <- { canvas | drawing <- drawing'} , history <- history', drawings <- Just drawings'}

        Viewing ->
          let moved = withinBounds editor'.canvas.dimensions <| stepMove ts <| getZoomable editor'
          in { editor' | absPos <- moved.absPos, lastPosition <- moved.lastPosition }

    _    -> editor'


moderatorState : Signal Moderator
moderatorState = foldp stepModerator defaultEditor input


toAbsPos zoom (dx, dy) (w, h) = (-(w / 2 + dx) * zoom, (h / 2 + dy) * zoom)


renderSubmittedDrawings : [DrawingInfo] -> Form
renderSubmittedDrawings ds =
  let
    getStrokes d = maybe [] id d.strokes
    strokes = foldl (\d ss -> (getStrokes d) ++ ss) [] ds
  in renderStrokes <| sortBy .t0 strokes




display : (Int, Int) -> Moderator -> Element
display (w, h) ({canvas, history, zoom, absPos, drawings} as board) =
  let
    thisStrokes = renderStrokes <| Dict.values canvas.drawing
    withOtherStrokes = maybe (filled (rgba 0 0 0 0) (circle 0)) renderSubmittedDrawings drawings
    displayCanvas = group [thisStrokes, withOtherStrokes]
    pos = toAbsPos zoom absPos <| floatT (w, h)
  in collage w h [ scale zoom <| move pos displayCanvas ]



main = display <~ Window.dimensions
                ~ moderatorState
