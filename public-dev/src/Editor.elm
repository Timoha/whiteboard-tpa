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
import Api (..)
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
            | FinishDrawing


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
  , mode : Mode
  , action : Action
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
port actionPort : Signal String
port userInfoPort : Signal (Maybe { drawingId : Int, firstName : String, lastName : String})
port canvasSizePort : Signal { width : Int, height : Int }
port boardInfoPort : Signal {instance : String, componentId : String, boardId : Int}
port submittedDrawingsPort : Signal [{drawingId:Int, firstName:String, lastName:String, strokes:Maybe [{id: Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}]

canvasSizePortToTuple : { width : Int, height : Int } -> (Int, Int)
canvasSizePortToTuple {width, height} = (width, height)


renderSubmittedDrawings : [DrawingInfo] -> Form
renderSubmittedDrawings ds =
  let
    getStrokes ds = case ds.strokes of
      Just ss -> ss
      Nothing -> []
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
    "FinishDrawing" -> FinishDrawing


withinWindowDims : [Touch.Touch] -> (Int, Int) -> [Touch.Touch]
withinWindowDims ts (w, h) =
  let within t = t.x > 0 && t.x < w && t.y > 0 && t.y < h
  in filter within ts


serverToAction : String -> (ServerAction, DrawingInfo)
serverToAction r =
  case Json.fromString r of
    Just v  -> let
                 res = (JS.toRecord . JS.fromJson) v
               in case res.action of
                  "AddPoints"    -> (AddPoints res.element, res.drawing)
                  "AddStrokes"   -> (AddStrokes res.element, res.drawing)
                  "RemoveStroke" -> (RemoveStroke res.element, res.drawing)
                  _              -> (NoOpServer, res.drawing)
    Nothing -> (NoOpServer, {firstName = "", lastName = "", drawingId = 0, strokes = Nothing}) -- throw error instead


isNoOpServer : ServerAction -> Bool
isNoOpServer m = case m of
  NoOpServer -> True
  _          -> False


brodcast : Signal ServerAction
brodcast =  merges [ dropRepeats . dropIf isNoOpServer NewClient  <| .lastMessage  <~ editorState
                   , drawingInfoToServerAction <~ (userInfoPortToDrawingInfo <~ dropRepeats userInfoPort)
                   ]


userInfoPortToDrawingInfo : Maybe { drawingId : Int, firstName : String, lastName : String} -> Maybe DrawingInfo
userInfoPortToDrawingInfo user =
  case user of
    Just u -> Just {u | strokes = Nothing}
    Nothing -> Nothing

drawingInfoToServerAction : Maybe DrawingInfo -> ServerAction
drawingInfoToServerAction user =
  case user of
    Just _  -> NewDrawing
    Nothing -> NewClient

outgoing : Signal String
outgoing = dropRepeats (constructMessage <~ brodcast ~ (dropRepeats <| userInfoPortToDrawingInfo <~ userInfoPort) ~ boardInfoPort)


incoming : Signal String
incoming = WebSocket.connect "ws://polar-refuge-5500.herokuapp.com/" outgoing


toAddDrawingsMessage : [DrawingInfo] -> (ServerAction, DrawingInfo)
toAddDrawingsMessage ds = (AddDrawings ds, {firstName = "", lastName = "", drawingId = 0, strokes = Nothing})


drawingInfosToOtherDrawings : [DrawingInfo] -> OtherDrawings
drawingInfosToOtherDrawings ds =
  let getStrokes d = case d.strokes of
        Just ss -> foldl (\s -> Dict.insert s.id s) Dict.empty ss
        Nothing -> Dict.empty
  in foldl (\d -> Dict.insert d.drawingId (getStrokes d)) Dict.empty ds



splitDrawings : Int -> [DrawingInfo] -> (Maybe DrawingInfo, [DrawingInfo])
splitDrawings did other =
  let
    (matched, rest) = partition (\x -> x.drawingId == did) other
  in if isEmpty matched then (Nothing, rest) else (Just <| head matched, rest)


getInitDrawings : Maybe DrawingInfo -> [DrawingInfo] -> (Drawing, OtherDrawings)
getInitDrawings dinfo drawings =
  let
    noCurrent = drawingInfosToOtherDrawings drawings
    getStrokes d = case d.strokes of
            Just ss -> foldl (\s -> Dict.insert s.id s) Dict.empty ss
            Nothing -> Dict.empty
  in case dinfo of
    Just info -> let
                   (d, other) = splitDrawings info.drawingId drawings
                 in case d of
                    Just d' -> (getStrokes d', drawingInfosToOtherDrawings other)
                    Nothing -> (Dict.empty, noCurrent)
    Nothing   -> (Dict.empty, noCurrent)


initDrawings : Signal (Drawing, OtherDrawings)
initDrawings = dropRepeats (getInitDrawings <~ (userInfoPortToDrawingInfo <~ (dropRepeats userInfoPort)) ~ (dropRepeats submittedDrawingsPort))
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
                          ~ dropRepeats (.mode <~ editorState)
                          ~ dropRepeats actions
-- OUTPUT


getDrawing : Realtime Whiteboard -> OtherDrawings -> Maybe ({ absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                  , drawing : [{id:Int, t0:Float, points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
getDrawing {mode, canvas, absPos, zoomOffset, zoom, windowDims} others =
  let
    getStrokes ds = case ds.strokes of
      Just ss -> ss
      Nothing -> []
    thisStrokes = Dict.values canvas.drawing
    withOtherStrokes = sortBy .t0 <| foldl (\d ss -> (Dict.values d) ++ ss) thisStrokes (Dict.values others)
    drawing = withOtherStrokes
  in case mode of
    Viewing -> Just {drawing = drawing, absPos = absPos, zoomOffset = zoomOffset, zoom = zoom, dimensions = canvas.dimensions, windowDims = windowDims}
    _ -> Nothing

port canvasOut : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                        , drawing : [{id:Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})
port canvasOut = getDrawing <~ editorState ~ othersState

port erasedDrawingIdsOut : Signal [Int]
port erasedDrawingIdsOut = dropRepeats (getErased <~ submittedDrawingsPort ~ submittedDrawingsPort)


getErased : [DrawingInfo] -> [DrawingInfo] -> [Int]
getErased orig current =
  let
    origIds = map .drawingId orig |> Set.fromList
    currIds = map .drawingId current |> Set.fromList
  in Set.diff origIds currIds |> Set.toList


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
    float (a, b) = (toFloat a, toFloat b)
    editor' = { editor | windowDims <- windowDims
                       , minZoom <- max 1 <| minScale (float windowDims) (float canvas.dimensions)
                       , canvas <- { canvas | dimensions <- canvasDims, drawing <- if isEmpty <| Dict.keys canvas.drawing then initDrawing else canvas.drawing }
                       , lastMessage <- NoOpServer }
  in case action of
    Undo ->
      let (drawing', history', message) = stepUndo canvas.drawing history
      in { editor' | canvas <- { canvas | drawing <- drawing' }, history <- history', lastMessage <- message}

    ZoomIn ->
      let zoomed = stepZoom 2 <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset }

    ZoomOut ->
      let zoomed = stepZoom (1/2) <| getZoomable editor'
      in { editor' | zoom <- zoomed.zoom, zoomOffset <- zoomed.zoomOffset }

    View ->
      { editor' | mode <- Viewing }

    Draw ->
      { editor' | mode <- Drawing }

    Erase ->
      { editor' | mode <- Erasing }

    Moderate ->
      { editor' | mode <- Moderating }

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

    NoOp    -> editor'


editorState : Signal (Realtime Whiteboard)
editorState = foldp stepEditor defaultEditor input


stepOther : OtherInput -> OtherDrawings -> OtherDrawings
stepOther {serverMessage, otherDrawings, mode, action } c =
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



display : (Int, Int) -> Realtime Whiteboard -> OtherDrawings -> Element
display (w, h) ({canvas, history, zoom, absPos} as board) o =
  let
    float (a, b) = (toFloat a, toFloat b)
    thisStrokes = Dict.values canvas.drawing
    withOtherStrokes = sortBy .t0 <| foldl (\d ss -> (Dict.values d) ++ ss) thisStrokes (Dict.values o)
    displayCanvas = renderStrokes withOtherStrokes
    toZero zoom (w, h) = (-w * zoom / 2, h * zoom / 2)
    toAbsPos (dx, dy) (x, y) = (x - dx * zoom, y + dy * zoom )
    pos = toAbsPos absPos <| toZero zoom (float (w, h))
  in collage w h [ scale zoom <| move pos displayCanvas ]



main = display <~ Window.dimensions
                ~ editorState
                ~ othersState
