module History where

import Dict
import Touch
import Canvas (Stroke, Drawing)

data Event = Erased [(Int, Stroke)] | Drew Int
type History = Dict.Dict Int Event

type Undoable a = { a | history : History }


recordDrew : [Touch.Touch] -> History -> History
recordDrew ts h = foldl (\t -> Dict.insert (abs t.id) (Drew <| abs t.id)) h ts




stepUndo : Drawing -> History -> (Drawing, History)
stepUndo drawing history  =
  let ids = Dict.keys history
  in if isEmpty ids
     then (drawing, history)
     else
       let
         lastId = maximum ids
       in case Dict.get lastId history of
           Nothing          -> ( Dict.empty, Dict.empty )
           Just (Drew id)   -> ( Dict.remove id drawing, Dict.remove id history )
           Just (Erased ss) -> ( foldl (\(id, s) d -> Dict.insert id s d) drawing ss
                               , foldl (\(id, s) h -> Dict.insert id (Drew id) h)
                                       (Dict.remove lastId history) ss )
