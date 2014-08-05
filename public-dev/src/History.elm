module History where

import Dict
import Touch
import Canvas (Stroke, Drawing, WithId, Point, Timed)
import Api (..)

data Event = Erased [(Int, Stroke)]
           | Drew Int
           | ErasedDrawings [DrawingInfo]

type History = Dict.Dict Int Event

type Undoable a = { a | history : History }


recordDrew : [Timed (WithId Point)] -> History -> History
recordDrew ps h = foldl (\p -> Dict.insert p.id (Drew p.id)) h ps




stepUndo : Drawing -> [DrawingInfo] -> History -> (Drawing, [DrawingInfo], History, ServerAction)
stepUndo drawing submitted history  =
  let ids = Dict.keys history
  in if isEmpty ids
     then (drawing, submitted, history, NoOpServer)
     else
       let
         lastId = maximum ids
       in case Dict.get lastId history of
           Nothing          -> ( Dict.empty, submitted, Dict.empty, NoOpServer )
           Just (Drew id)   -> ( Dict.remove id drawing, submitted, Dict.remove id history, RemoveStroke { strokeId = id } )
           Just (Erased ss) -> ( foldl (\(id, s) d -> Dict.insert id s d) drawing ss
                               , submitted
                               , foldl (\(id, s) h -> Dict.insert id (Drew id) h)
                                       (Dict.remove lastId history) ss
                               , AddStrokes { strokes = map (\(id, s) -> { s | id = id }) ss } )
           Just (ErasedDrawings ds)
               -> ( drawing
                  , submitted ++ ds
                  , Dict.remove lastId history
                  , NoOpServer )
