module Minimap where

import Utils (..)
import Canvas (renderStrokes)
import Window
import Dict
import Debug



type PortCanvas = { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
           , drawing : [{id: Int, t0:Float, points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}

port canvasPort : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                , drawing : [{id: Int, t0:Float, points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})

scaleToMinimap : (Float, Float) -> (Float, Float) -> (Float, (Float, Float))
scaleToMinimap (w, h) (winW, winH) =
  let factor = winW / w
  in (factor, (winW, h * factor))


borderLine = { defaultLine | width <- 3, color <- rgb 41 171 226, join <- Clipped }

display : (Int, Int) -> Maybe PortCanvas -> Element
display thisWindowDims c =
  case c of
    Nothing -> spacer 0 0
    Just {drawing, absPos, zoomOffset, dimensions, windowDims, zoom} ->
      let
        scaleDim f (w, h) = (w * f, h * f)
        (zoom', (w, h)) = scaleToMinimap (floatT dimensions) (floatT thisWindowDims)
        (bw, bh) = scaleF (zoom / zoom') <| floatT windowDims
        leftTop = flipVert <| scaleDim zoom' <| addT absPos zoomOffset
        zeroPos = addT (-(w - bw) / 2, (h - bh) / 2) leftTop
        border = move zeroPos <| outlined borderLine (rect bw bh)
        canvas = scale zoom' <| move (-w / 2, h / 2) <| renderStrokes <| sortBy .t0 <|drawing
      in collage (round w) (round h) [canvas, border]



main = display <~ Window.dimensions
                ~ canvasPort
