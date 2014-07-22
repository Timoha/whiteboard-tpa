module Eraser where

import Dict
import Touch
import Debug
import History (History, Drew, Erased, Event)
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



removeEraser : Drawing -> History -> (Drawing, History)
removeEraser drawing history =
  let ids = Dict.keys history
  in if isEmpty ids
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




stepEraser : [Brushed Touch.Touch] -> Drawing -> History -> (Drawing, History)
stepEraser ts drawing history =
  if isEmpty ts
  then removeEraser drawing history
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
                 in ( foldl (\(eid, _) -> Dict.remove eid) drawing' erased
                    , Dict.insert id (Erased <| erased ++ es) history )
      Nothing -> ( drawing'
                 , Dict.insert id (Erased []) history )
