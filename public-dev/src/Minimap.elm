module Minimap where

import Canvas (Canvas, pointToTuple, thickLine, dot, renderStrokes)
import Navigation (Zoomable)
import Window
import Dict
import Debug



type PortCanvas = { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
           , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}

port canvasPort : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})

scaleToMinimap : (Float, Float) -> (Float, Float) -> (Float, (Float, Float))
scaleToMinimap (w, h) (winW, winH) =
  let
    factor = winW / w
  in (factor, (winW, h * factor))


display : (Int, Int) -> Maybe PortCanvas -> Element
display thisWindowDims c =
  case c of
    Nothing -> spacer 0 0
    Just {drawing, absPos, zoomOffset, dimensions, windowDims, zoom} ->
      let
        float (a, b) = (toFloat a, toFloat b)
        flipVert (a, b) = (a, -b)
        addT (x, y) (dx, dy) = (x + dx, y + dy)
        scaleDim f (w, h) = (w * f, h * f)
        (zoom', (w, h)) = scaleToMinimap (float dimensions) (float thisWindowDims)
        (bw, bh) = scaleDim (zoom' / zoom) <| float windowDims
        borderLine = { defaultLine | width <- 3, color <- rgb 41 171 226, join <- Clipped }
        leftTop = flipVert <| scaleDim zoom' <| addT absPos zoomOffset
        zeroPos = addT (-(w - bw) / 2, (h - bh) / 2) leftTop
        border = move zeroPos <| outlined borderLine (rect bw bh)
        canvas = scale zoom' <| move (-w / 2, h / 2) <| renderStrokes drawing
      in collage (round w) (round h) [canvas, border]



main = display <~ Window.dimensions
                ~ canvasPort
