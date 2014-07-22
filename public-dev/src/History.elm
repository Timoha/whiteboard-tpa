module History where

data Event = Erased [Stroke] | Drew Int
type History = Dict.Dict Int Event

type Undoable a = { a | history : History }


recordDrew : [Touch.Touch] -> History -> History
recordDrew ts h = foldl (\t -> Dict.insert (abs t.id) (Drew <| abs t.id)) h ts


stepUndo : Canvas -> Canvas
stepUndo ({drawing, history} as c)  =
  let ids = Dict.keys history
  in if isEmpty ids
     then c
     else
       let
         lastId = maximum ids
         (d, h) = case Dict.get lastId history of
           Nothing          -> ( Dict.empty, Dict.empty )
           Just (Drew id)   -> ( Dict.remove id drawing, Dict.remove id history )
           Just (Erased ss) -> ( foldl (\s d -> Dict.insert s.id s d) drawing ss
                               , foldl (\s h -> Dict.insert s.id (Drew s.id) h)
                                       (Dict.remove lastId history) ss )
      in { c | drawing <- d, history <- h }