module Navigation where

import Utils (..)

import Touch
import Debug

type Zoomable a = { a | windowDims : (Int, Int)
                      , zoom : Float
                      , minZoom : Float
                      , maxZoom : Float
                      , absPos : (Float, Float)
                      , zoomOffset : (Float, Float)
                      , lastPosition : Maybe (Int, Int) }



stepMove : [Touch.Touch] -> Zoomable a -> Zoomable a
stepMove ts ({lastPosition, zoom, absPos} as o) =
  let
    (x, y) = absPos
  in if isEmpty ts
  then { o | lastPosition <- Nothing }
  else
    let
      t = head ts
    in case lastPosition of
       Just (tx, ty) ->
         let
            (dx, dy) = floatT (tx - t.x, ty - t.y)
            x' = x + dx / zoom
            y' = y + dy / zoom
         in { o | lastPosition <- Just (t.x, t.y)
                , absPos  <- (x', y')}
       Nothing -> { o | lastPosition <- Just (t.x, t.y) }



minScale : (Float, Float) -> (Float,Float) -> Float
minScale (winW, winH) (w,h) = max (winW / w) (winH / h)


withinBounds : (Int, Int) -> Zoomable a -> Zoomable a
withinBounds dimensions ({zoom, absPos, zoomOffset, windowDims} as o) =
  let
    limitLeftTop (x, y) (x', y') = (max x x', max y y')
    limitRightBottom (x, y) (x', y') = (min x x', min y y')
    leftTop = limitLeftTop (addT absPos zoomOffset) (0, 0)
    windowDims' = scaleF zoom <| floatT windowDims
    (w, h) = floatT dimensions
    (right, bottom) = limitRightBottom (addT leftTop windowDims') (w, h)
    absPos' = if right < w && bottom < h
              then subT leftTop zoomOffset
              else subT (subT (right, bottom) windowDims') zoomOffset
  in { o | absPos <- absPos' }



stepZoom : Float -> Zoomable a -> Zoomable a
stepZoom factor ({windowDims, zoom, minZoom, maxZoom, zoomOffset} as o) =
  let
    dimensions = floatT windowDims
    zoom' = min (max (zoom * factor) minZoom) maxZoom
    winD  = scaleF zoom dimensions
    winD' = scaleF zoom' dimensions
    (dx, dy) = scaleF 2 <| subT winD winD'
    (x, y) = zoomOffset
  in { o | zoom    <- zoom'
         , zoomOffset <- (x + dx, y + dy)}



scaleTouch : (Float, Float) -> (Float, Float) -> Float -> Touch.Touch -> Touch.Touch
scaleTouch (x, y) (dx, dy) zoom t =
  let
    (tx, ty) = floatT (t.x, t.y)
  in { t | x <- round (tx / zoom + x + dx)
         , y <- round (ty / zoom + y + dy) }
