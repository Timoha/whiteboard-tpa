module Eraser where

import Dict
import Touch
import Debug
import Canvas (..)


ccw : Point -> Point -> Point -> Bool
ccw a b c = (c.y - a.y) * (b.x-a.x) > (b.y - a.y) * (c.x - a.x)

{- Thanks to http://stackoverflow.com/a/9997374 -}

isIntersect : Line -> Line -> Bool
isIntersect l1 l2 = not <|
  (ccw l1.p1 l2.p1 l2.p2) == (ccw l1.p2 l2.p1 l2.p2) ||
  (ccw l1.p1 l1.p2 l2.p1) == (ccw l1.p1 l1.p2 l2.p2)


toSegments : [Point] -> [Line]
toSegments ps =
  let
    connectPrev p2 ps = case ps of
      [] -> startNew p2 []
      ps -> (line (head ps).p1 p2) :: tail ps
    startNew p1 ls = (line p1 p1) :: ls
    connect p ls = startNew p <| connectPrev p ls
  in tail <| foldl connect [] ps


isLineStrokeIntersect : Line -> Stroke -> Bool
isLineStrokeIntersect l s =
  if (length s.points) > 1
  then any (isIntersect l) <| toSegments s.points
  else False



removeEraser : Canvas -> Canvas
removeEraser ({drawing, history} as c) =
  let
    ids = Dict.keys history
    (d, h) = if isEmpty ids
             then ( Dict.empty,  Dict.empty )
             else
               let
                 lastId = maximum ids
                 removeLast = Dict.remove lastId
               in case Dict.get lastId history of
                 Just (Erased ss) -> if isEmpty ss
                                     then ( removeLast drawing
                                          , removeLast history )
                                     else ( removeLast drawing
                                          , history )
                 _                -> ( drawing, history )
  in { c | drawing <- d, history <- h }




stepEraser : [Brushed Touch.Touch] -> Canvas -> Canvas
stepEraser ts ({drawing, history} as c) =
  if isEmpty ts
  then removeEraser c
  else
    let
      t = head ts
      id = abs t.id
      drawing' = stepStroke t drawing
    in case Dict.get id drawing of
      Just s  -> let
                   eraserSeg = line (point t.x t.y) (head s.points)
                   strokes = tail . reverse <| Dict.toList drawing
                   crossed = filter (\(id, s) -> isLineStrokeIntersect eraserSeg s) strokes
                   erased = if isEmpty crossed then [] else [head crossed] -- erase only latest
                   es = case Dict.get id history of
                     Just (Erased ss) -> ss
                     _                -> []
                 in {c | drawing <- foldl (\(eid, _) -> Dict.remove eid) drawing' erased
                       , history <- Dict.insert id (Erased <| erased ++ es) history }
      Nothing -> {c | drawing <- drawing'
                    , history <- Dict.insert id (Erased []) history}
