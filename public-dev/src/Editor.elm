module Editor where

import Dict
import Set
import Touch
import Window
import Debug
import Navigation (..)
import Canvas (..)
import Eraser (stepEraser, stepModerating)
import History (..)
import RealtimeApi (..)
import Utils (..)
import JavaScript.Experimental as JS
import Json
import WebSocket
import Maybe


import Graphics.Input (..)
import Graphics.Input as Input




-- MODEL

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
  , brush      : Brush
  , canvasDims : (Int, Int)
  , windowDims : (Int, Int)
  , initDrawing : Drawing
  }

type OtherInput =
  { serverMessage : (ServerAction, DrawingInfo)
  , otherDrawings : OtherDrawings
  }

type Whiteboard =  Zoomable (Undoable Editor)

type Editor = { mode : Mode, canvas : Canvas }

type Realtime a = {a | lastMessage : ServerAction}

type OtherDrawings = Dict.Dict Int Drawing

defaultCanvas : Canvas
defaultCanvas =
  { drawing = Dict.empty
  , dimensions = (0, 0)
  }


defaultEditor : Realtime Whiteboard
defaultEditor =
  { canvas = defaultCanvas
  , mode = Viewing
  , history = Dict.empty
  , zoom = 1
  , minZoom = 1
  , maxZoom = 2 ^ 4
  , zoomOffset = (0, 0)
  , absPos = (0, 0)
  , windowDims = (0, 0)
  , lastPosition = Nothing
  , lastMessage = NewClient
  }


defaultOther : OtherDrawings
defaultOther = Dict.empty

-- INPUT

port brushPort : Signal { size : Float, color : { red : Int, green : Int, blue : Int, alpha : Float }  }
port userInfoPort : Signal (Maybe { drawingId : Int, firstName : String, lastName : String})
port actionPort : Signal String
port canvasSizePort : Signal { width : Int, height : Int }
port boardInfoPort : Signal {instance : String, componentId : String, boardId : Int}
port submittedDrawingsPort : Signal (Maybe [{drawingId:Int, firstName:String, lastName:String, strokes:Maybe [{id: Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}])

canvasSizePortToTuple : { width : Int, height : Int } -> (Int, Int)
canvasSizePortToTuple {width, height} = (width, height)


renderSubmittedDrawings : [DrawingInfo] -> Form
renderSubmittedDrawings ds =
  let
    getStrokes d = maybe [] id d.strokes
    strokes = foldl (\d ss -> (getStrokes d) ++ ss) [] ds
  in renderStrokes <| sortBy .t0 strokes

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



brodcast : Signal ServerAction
brodcast =  merges [ dropRepeats . dropIf isNoOpServer NewClient  <| .lastMessage  <~ editorState
                   , drawingInfoToServerAction <~ (userInfoPortToDrawingInfo <~ dropRepeats userInfoPort)
                   ]


userInfoPortToDrawingInfo : Maybe { drawingId : Int, firstName : String, lastName : String} -> Maybe DrawingInfo
userInfoPortToDrawingInfo user =
  case user of
    Just u -> Just {u | strokes = Nothing}
    Nothing -> Nothing



outgoing : Signal String
outgoing = dropRepeats (constructMessage <~ brodcast ~ (dropRepeats <| userInfoPortToDrawingInfo <~ userInfoPort) ~ boardInfoPort)


incoming : Signal String
incoming = WebSocket.connect "ws://localhost:3000/" outgoing


toAddDrawingsMessage : [DrawingInfo] -> (ServerAction, DrawingInfo)
toAddDrawingsMessage ds = (AddDrawings ds, {firstName = "", lastName = "", drawingId = 0, strokes = Nothing})


drawingInfosToOtherDrawings : [DrawingInfo] -> OtherDrawings
drawingInfosToOtherDrawings ds =
  let getStrokes d =  maybe Dict.empty (foldl (\s -> Dict.insert s.id s) Dict.empty) d.strokes
  in foldl (\d -> Dict.insert d.drawingId (getStrokes d)) Dict.empty ds



splitDrawings : Int -> [DrawingInfo] -> (Maybe DrawingInfo, [DrawingInfo])
splitDrawings did other =
  let
    (matched, rest) = partition (\x -> x.drawingId == did) other
  in if isEmpty matched then (Nothing, rest) else (Just <| head matched, rest)


getInitDrawings : Maybe DrawingInfo -> Maybe [DrawingInfo] -> (Drawing, OtherDrawings)
getInitDrawings dinfo drawings =
  let
    ds = maybe [] id drawings
    noCurrent = drawingInfosToOtherDrawings ds
    getStrokes d = maybe Dict.empty (foldl (\s -> Dict.insert s.id s) Dict.empty) d.strokes
  in case dinfo of
    Just info -> let
                   (d, other) = splitDrawings info.drawingId ds
                 in case d of
                    Just d' -> (getStrokes d', drawingInfosToOtherDrawings other)
                    Nothing -> (Dict.empty, noCurrent)
    Nothing   -> (Dict.empty, noCurrent)


initDrawings : Signal (Drawing, OtherDrawings)
initDrawings = dropRepeats (getInitDrawings <~ (userInfoPortToDrawingInfo <~ (dropRepeats userInfoPort)) ~ submittedDrawingsPort)
--toolActions : Input Action
--toolActions = Input.input NoOp

sortTouches : [Touch.Touch] -> [Touch.Touch]
sortTouches ts = reverse <| sortBy .t0 ts -- temp solution for zombie touches

actions : Signal Action
actions = merges [ Touches <~ (withinWindowDims <~ (Touch.touches) ~ Window.dimensions)
                 , portToAction <~ actionPort
                 --, toolActions.signal
                 ]


input : Signal Input
input = Input <~ actions
               ~ dropRepeats brushPort
               ~ dropRepeats (canvasSizePortToTuple <~ canvasSizePort)
               ~ dropRepeats Window.dimensions
               ~ dropRepeats (fst <~ initDrawings)




serverInput : Signal OtherInput
serverInput = OtherInput <~ dropRepeats (serverToAction <~ incoming)
                          ~ dropRepeats (snd <~ initDrawings)
-- OUTPUT


getDrawing : Realtime Whiteboard -> OtherDrawings -> Maybe ({ absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                  , drawing : [{id:Int, t0:Float, points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
getDrawing {mode, canvas, absPos, zoomOffset, zoom, windowDims} other =
  let withOtherStrokes = sortBy .t0 <| foldl (\d ss -> (Dict.values d) ++ ss) (Dict.values canvas.drawing) (Dict.values other)
  in case mode of
    Viewing -> Just {drawing = withOtherStrokes, absPos = absPos, zoomOffset = zoomOffset, zoom = zoom, dimensions = canvas.dimensions, windowDims = windowDims}
    _ -> Nothing

port canvasOut : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                        , drawing : [{id:Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
port canvasOut = getDrawing <~ editorState ~ othersState


-- UPDATE

getZoomable : Realtime Whiteboard -> Zoomable Editor
getZoomable w = { canvas = w.canvas
                , mode = w.mode
                , windowDims = w.windowDims
                , zoom = w.zoom
                , minZoom = w.minZoom
                , maxZoom = w.maxZoom
                , absPos = w.absPos
                , zoomOffset = w.zoomOffset
                , lastPosition = w.lastPosition }


stepMode : Action -> Mode -> Mode
stepMode action mode =
  case action of
    View  -> Viewing
    Draw  -> Drawing
    Erase -> Erasing
    _     -> mode



eraserBrush : Brush
eraserBrush = { size = 15, color = toRgb (rgba 0 0 0 0.1) }




stepEditor : Input -> Realtime Whiteboard -> Realtime Whiteboard
stepEditor {action, brush, canvasDims, windowDims, initDrawing}
           ({mode, canvas, history, zoom, absPos, zoomOffset} as editor) =
  let
    editor' = { editor | windowDims <- windowDims
                       , minZoom <- max 1 <| minScale (floatT windowDims) (floatT canvas.dimensions)
                       , canvas <- { canvas | dimensions <- canvasDims, drawing <- if isEmpty <| Dict.keys canvas.drawing then initDrawing else canvas.drawing }
                       , lastMessage <- NoOpServer }
  in case action of
    Undo ->
      let (drawing', history', message) = stepUndo canvas.drawing history
      in { editor' | canvas <- { canvas | drawing <- drawing' }, history <- history', lastMessage <- message}

    ZoomIn ->
      let zoomed = withinBounds editor'.canvas.dimensions <| stepZoom 2 <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset, absPos <- zoomed.absPos }

    ZoomOut ->
      let zoomed = withinBounds editor'.canvas.dimensions <| stepZoom (1/2) <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset, absPos <- zoomed.absPos }

    View ->
      { editor' | mode <- Viewing }

    Draw ->
      { editor' | mode <- Drawing }

    Erase ->
      { editor' | mode <- Erasing }

    Touches ts ->
      let ps = map (touchToPoint . (scaleTouch absPos zoomOffset zoom)) ts
      in case mode of
        Drawing ->
          { editor' | canvas <- { canvas | drawing <- addN (applyBrush ps brush) canvas.drawing }
                    , history <- recordDrew ps history
                    , lastMessage <- AddPoints { brush = brush, points = ps } }

        Erasing ->
          let (drawing', history', message) = stepEraser (applyBrush ps eraserBrush) canvas.drawing history
          in { editor' | canvas <- { canvas | drawing <- drawing'} , history <- history', lastMessage <- message}

        Viewing ->
          let moved = withinBounds editor'.canvas.dimensions <| stepMove ts <| getZoomable editor'
          in { editor' | absPos <- moved.absPos, lastPosition <- moved.lastPosition }

    _    -> editor'


editorState : Signal (Realtime Whiteboard)
editorState = foldp stepEditor defaultEditor input


stepOther : OtherInput -> OtherDrawings -> OtherDrawings
stepOther {serverMessage, otherDrawings} c =
  let
    (a, d) = serverMessage
    canvas = if isEmpty (Dict.keys c) then otherDrawings else c
    drawing = Dict.getOrElse Dict.empty d.drawingId c
    drawing' = case a of
      AddPoints ps   -> addN (applyBrush ps.points ps.brush) drawing
      AddStrokes ss  -> foldl (\s -> Dict.insert s.id s) drawing ss.strokes
      RemoveStroke s -> Dict.remove s.strokeId drawing
      _              -> drawing
  in Dict.insert d.drawingId drawing' canvas


othersState : Signal OtherDrawings
othersState =  dropRepeats (foldp stepOther defaultOther serverInput)


-- VIEW


toAbsPos zoom (dx, dy) (w, h) = ( -zoom * (w / 2 + dx), zoom * (h / 2 + dy) )


display : (Int, Int) -> Realtime Whiteboard -> OtherDrawings -> Element
display (w, h) ({canvas, history, zoom, absPos} as board) other =
  let
    thisStrokes = Dict.values canvas.drawing
    withOtherStrokes = sortBy .t0 <| foldl (\d ss -> (Dict.values d) ++ ss) thisStrokes (Dict.values other)
    displayCanvas = renderStrokes withOtherStrokes
    pos = toAbsPos zoom absPos <| floatT (w, h)
  in collage w h [ scale zoom <| move pos displayCanvas ]



main = display <~ Window.dimensions
                ~ editorState
                ~ othersState
