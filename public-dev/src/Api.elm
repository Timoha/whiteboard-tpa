module Api where

import Json
import Dict
import Canvas (Point, Brush, Stroke, WithId, Timed)
import JavaScript.Experimental as JS





type DrawingInfo =
  { firstName : String
  , lastName : String
  , drawingId : Int
  , strokes : Maybe [Stroke]
  }

type BoardInfo =
  { instance : String
  , componentId : String
  , boardId : Int
  }


data ServerAction = AddPoints { brush:Brush, points:[Timed (WithId Point)] }
                  | AddStrokes { strokes:[Stroke] }
                  | RemoveStroke { strokeId:Int }
                  | AddDrawings { online:[{drawingId:Int, firstName:String, lastName:String, strokes:[{id: Int, t0:Float,  points:[{ x:Int, y:Int }], brush:{ size:Float, color:{ red:Int, green:Int, blue:Int, alpha:Float }}}]}]}
                  | NewClient
                  | NoOpServer





constructMessage : ServerAction -> Maybe DrawingInfo -> BoardInfo -> String
constructMessage a u w = Json.toString "" <| jsonOfServerAction a u w

jsonOfServerAction : ServerAction -> Maybe DrawingInfo -> BoardInfo -> Json.Value
jsonOfServerAction action dinfo binfo =
  let
    recordToJson = (JS.toJson . JS.fromRecord)
  in case dinfo of
    Just d ->
        let
          jav = case action of
              AddPoints ps   -> [("action", Json.String "AddPoints"), ("element", recordToJson ps)]
              AddStrokes ss  -> [("action", Json.String "AddStrokes"), ("element", recordToJson ss)]
              RemoveStroke s -> [("action", Json.String "RemoveStroke"), ("element", recordToJson s)]
              NoOpServer     -> [("action", Json.String "NoOpServer"), ("element", Json.Null)]
              NewClient      -> [("action", Json.String "NewClient"), ("element", Json.Null)]
        in toJsonObj <| [("board", recordToJson binfo), ("drawing", recordToJson d)] ++ jav
    Nothing -> toJsonObj <| [("board", recordToJson binfo), ("action", Json.String "NewClient"), ("element", Json.Null), ("drawing", Json.Null)]


toJsonObj : [(String, Json.Value)] -> Json.Value
toJsonObj ps = Json.Object <| Dict.fromList ps







