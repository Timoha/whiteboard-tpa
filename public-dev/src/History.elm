module History where

import Dict
import Touch
import Canvas (Stroke, Drawing)
import Api (..)

data Event = Erased [(Int, Stroke)]
           | Drew Int

type History = Dict.Dict Int Event

type Undoable a = { a | history : History }


recordDrew : [Touch.Touch] -> History -> History
recordDrew ts h = foldl (\t -> Dict.insert (abs t.id) (Drew <| abs t.id)) h ts




stepUndo : Drawing -> History -> (Drawing, History, ServerAction)
stepUndo drawing history  =
  let ids = Dict.keys history
  in if isEmpty ids
     then (drawing, history, NoOpServer)
     else
       let
         lastId = maximum ids
       in case Dict.get lastId history of
           Nothing          -> ( Dict.empty, Dict.empty, NoOpServer )
           Just (Drew id)   -> ( Dict.remove id drawing, Dict.remove id history, RemoveStroke { strokeId = id } )
           Just (Erased ss) -> ( foldl (\(id, s) d -> Dict.insert id s d) drawing ss
                               , foldl (\(id, s) h -> Dict.insert id (Drew id) h)
                                       (Dict.remove lastId history) ss
                               , AddStrokes { strokes = map (\(id, s) -> {id = id, stroke = s}) ss } )
