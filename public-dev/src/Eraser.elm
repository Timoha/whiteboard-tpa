module Eraser where

import Dict
import Touch
import History (History, Drew, Erased, Event, ErasedDrawings)
import Canvas (..)
import Api (..)
import JavaScript.Experimental as JS
import Debug



ccw : Point -> Point -> Point -> Bool
ccw a b c = (c.y - a.y) * (b.x-a.x) > (b.y - a.y) * (c.x - a.x)

{- Thanks to http://stackoverflow.com/a/9997374 -}

isIntersect : Line -> Line -> Bool
isIntersect l1 l2 = not <|
  (ccw l1.p1 l2.p1 l2.p2) == (ccw l1.p2 l2.p1 l2.p2) ||
  (ccw l1.p1 l1.p2 l2.p1) == (ccw l1.p1 l1.p2 l2.p2)


distToLineSquared : Point -> Line -> Float
distToLineSquared p l =
  let
    lengthSquared l = toFloat <| (l.p1.x - l.p2.x)^2 + (l.p1.y - l.p2.y)^2
    t = (toFloat ((p.x - l.p1.x) * (l.p2.x - l.p1.x) + (p.y - l.p1.y) * (l.p2.y - l.p1.y))) / (lengthSquared l)
  in if | t < 0 -> lengthSquared <| line p l.p1
        | t > 1 -> lengthSquared <| line p l.p2
        | otherwise -> let
                         p1 = l.p1.x + (round <| t * (toFloat (l.p2.x - l.p1.x)))
                         p2 = l.p1.y + (round <| t * (toFloat (l.p2.y - l.p1.y)))
                         projection = point p1 p2
                       in lengthSquared <| line p projection


toSegments : [Point] -> [Line]
toSegments ps =
  let
    connectPrev p2 ps = case ps of
      [] -> startNew p2 []
      ps -> (line (head ps).p1 p2) :: tail ps
    startNew p1 ls = (line p1 p1) :: ls
    connect p ls = startNew p <| connectPrev p ls
  in tail <| foldl connect [] ps


eraserRadiusSquared : Float
eraserRadiusSquared = 7.5 ^ 2


isLineStrokeIntersect : Line -> Stroke -> Bool
isLineStrokeIntersect l s =
  if (length s.points) > 1
  then any (isIntersect l) <| toSegments s.points
  else (distToLineSquared (head s.points) l) < (s.brush.size / 2) ^ 2



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
         Just (ErasedDrawings ds) -> if isEmpty ds
                             then ( removeLast drawing
                                  , removeLast history )
                             else ( removeLast drawing
                                  , history )
         _                -> ( drawing, history )




stepEraser : [Brushed (Timed (WithId Point))] -> Drawing -> History -> (Drawing, History, ServerAction)
stepEraser ps drawing history =
  if isEmpty ps
  then let (drawing', history') = removeEraser drawing history in (drawing', history', NoOpServer)
  else
    let
      p = head ps
      drawing' = stepStroke p drawing
    in case Dict.get p.id drawing of
      Just s  -> let
                   eraserSeg = line (point p.x p.y) (head s.points)
                   strokes = tail . reverse <| Dict.toList drawing
                   crossed = filter (\(_, s) -> isLineStrokeIntersect eraserSeg s) strokes
                   erased = if isEmpty crossed then [] else [head crossed] -- erase only latest
                   es = case Dict.get p.id history of
                     Just (Erased ss) -> ss
                     _                -> []
                 in ( foldl (\(eid, _) -> Dict.remove eid) drawing' erased
                    , Dict.insert p.id (Erased <| erased ++ es) history
                    , if isEmpty crossed then NoOpServer else RemoveStroke { strokeId = (fst . head) crossed })
      Nothing -> ( drawing'
                 , Dict.insert p.id (Erased []) history
                 , NoOpServer)


stepModerating : [Brushed (Timed (WithId Point))] -> Drawing -> [DrawingInfo] -> History -> (Drawing, [DrawingInfo], History)
stepModerating ps drawing submitted history =
  if isEmpty ps
  then let (drawing', history') = removeEraser drawing history in (drawing', submitted, history')
  else
    let
      p = head ps
      drawing' = stepStroke p drawing
    in case Dict.get p.id drawing of
      Just s  -> let
                   eraserSeg = line (point p.x p.y) (head s.points)
                   isInsersect d = case d.strokes of
                      Just ss -> any (isLineStrokeIntersect eraserSeg) ss
                      Nothing -> False
                   crossed = filter isInsersect submitted
                   erased = if isEmpty crossed then [] else [head crossed] -- erase only latest
                   ed = case Dict.get p.id history of
                     Just (ErasedDrawings ds) -> ds
                     _                        -> []
                 in ( drawing'
                    , if isEmpty crossed then submitted else filter (\d -> d.drawingId /= (head erased).drawingId) submitted
                    , Dict.insert p.id (ErasedDrawings <| erased ++ ed) history )
      Nothing -> ( drawing'
                 , submitted
                 , Dict.insert p.id (ErasedDrawings []) history )

