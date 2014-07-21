module Minimap where

import Canvas (Canvas, pointToTuple, thickLine, dot)
import Navigation (Zoomable)
import Window
import Dict
import Debug



type Can = { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
           , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}

port canvasPort : Signal (Maybe { absPos : (Float, Float), zoomOffset : (Float, Float), zoom : Float, dimensions : (Int, Int), windowDims : (Int, Int)
                                , drawing : [{ points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]})

scaleToMinimap : (Int, Int) -> (Int, Int) -> (Float, (Int, Int))
scaleToMinimap dims winDims =
  let
    float (a, b) = (toFloat a, toFloat b)
    roundT (a, b) = (round a, round b)
    (w, h) = float dims
    (winW, winH) = float winDims
    factor = winW / w
  in (factor, roundT (winW, h * factor))


display : (Int, Int) -> Maybe Can -> Element
display winDims c =
  case c of
    Nothing -> spacer 0 0
    Just {drawing, absPos, zoomOffset, dimensions, windowDims, zoom} -> let
            float (a, b) = (toFloat a, toFloat b)
            flipVert (a, b) = (a, -b)
            negate (a, b) = (-a, b)
            addT (x, y) (dx, dy) = (x + dx, y + dy)
            scaleDim f (w, h) = (w/f, h/f)
            paths = drawing
            strokeOrDot p =
              if (length p.points) > 1
              then traced (thickLine p.brush) <| map (flipVert . pointToTuple) p.points
              else dot (flipVert . pointToTuple . head <| p.points) p.brush
            (fw, fh) = scaleDim zoom <| float windowDims
            frame = move (flipVert <| addT absPos zoomOffset) <| move (fw / 2, -fh / 2) <| outlined { defaultLine | width <- 45, color <- lightBlue } (rect fw fh)
            forms = (map strokeOrDot paths) ++ [frame]
            toZero (w, h) = (-w / 2, h / 2)
            (zoom', (w, h)) = scaleToMinimap dimensions winDims
            pos = toZero (float (w, h))
          in collage w h [ scale zoom' <| move pos (group forms) ]



main = display <~ Window.dimensions
                ~ canvasPort
