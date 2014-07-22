module Navigation where

import Touch

type Zoomable a = { a | windowDims : (Int, Int)
                      , zoom : Float
                      , minZoom : Float
                      , maxZoom : Float
                      , absPos : (Float, Float)
                      , zoomOffset : (Float, Float)
                      , lastMove : Maybe (Int, Int) }



stepMove : [Touch.Touch] -> Zoomable a -> Zoomable a
stepMove ts ({lastMove, zoom, absPos} as o) =
  let
    (x, y) = absPos
  in if isEmpty ts
  then { o | lastMove <- Nothing }
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
         in { o | lastMove <- Just (t.x, t.y)
                , absPos  <- (x', y')}
       Nothing -> { o | lastMove <- Just (t.x, t.y) }



minScale : (Float, Float) -> (Float,Float) -> Float
minScale (winW, winH) (w,h) =
  max (winW / w) (winH / h)


withinBounds : (Int, Int) -> Zoomable a -> Zoomable a
withinBounds dimensions ({zoom, absPos, zoomOffset, windowDims} as o) =
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
  in { o | absPos <- absPos' }



stepZoom : Float -> Zoomable a -> Zoomable a
stepZoom factor ({windowDims, zoom, minZoom, maxZoom, zoomOffset} as o) =
  let
    scaleF f (a, b) = (a / f, b / f)
    float (a, b) = (toFloat a, toFloat b)
    delta (a, b) (a', b') = (a - a', b - b')
    zoom' = min (max (zoom * factor) minZoom) maxZoom
    winD  = scaleF zoom <| float windowDims
    winD' = scaleF zoom' <| float windowDims
    (dx, dy) = scaleF 2 <| delta winD winD'
    (x, y) = zoomOffset
  in { o | zoom    <- zoom'
         , zoomOffset <- (x + dx, y + dy)}



scaleTouches : (Float, Float) -> (Float, Float) -> Float -> Touch.Touch -> Touch.Touch
scaleTouches (x, y) (dx, dy) zoom t =
  let
    float (a, b) = (toFloat a, toFloat b)
    (tx, ty) = float (t.x, t.y)
  in { t | x <- round (tx / zoom + x + dx)
         , y <- round (ty / zoom + y + dy) }
