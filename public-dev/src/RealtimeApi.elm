module RealtimeApi where

import Json
import Dict
import Canvas (Point, Brush, Stroke, WithId, Timed)
import JavaScript.Experimental as JS
import Debug


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
                  | AddDrawings [DrawingInfo]
                  | NewDrawing
                  | NewClient
                  | NoOpServer


isNoOpServer : ServerAction -> Bool
isNoOpServer m = case m of
  NoOpServer -> True
  _          -> False


drawingInfoToServerAction : Maybe DrawingInfo -> ServerAction
drawingInfoToServerAction user =
  case user of
    Just _  -> NewDrawing
    Nothing -> NewClient


serverToAction : String -> (ServerAction, DrawingInfo)
serverToAction r =
  case Json.fromString r of
    Just v  -> let
                 res = (JS.toRecord . JS.fromJson) v
               in case res.action of
                  "AddPoints"    -> (AddPoints res.element, res.drawing)
                  "AddStrokes"   -> (AddStrokes res.element, res.drawing)
                  "RemoveStroke" -> (RemoveStroke res.element, res.drawing)
                  _              -> (NoOpServer, res.drawing)
    Nothing -> (NoOpServer, {firstName = "", lastName = "", drawingId = 0, strokes = Nothing}) -- throw error instead



constructMessage : ServerAction -> Maybe DrawingInfo -> BoardInfo -> String
constructMessage a u w = Json.toString "" <| jsonOfServerAction a u w


-- REFACTOR ALL THIS STUFFF
jsonOfServerAction : ServerAction -> Maybe DrawingInfo -> BoardInfo -> Json.Value
jsonOfServerAction action dinfo binfo =
  let
    recordToJson = (JS.toJson . JS.fromRecord)
  in case dinfo of
    Just d ->
        let
          jav = case Debug.log "server action" <| action of
              AddPoints ps   -> [("action", Json.String "AddPoints"), ("element", recordToJson ps)]
              AddStrokes ss  -> [("action", Json.String "AddStrokes"), ("element", recordToJson ss)]
              RemoveStroke s -> [("action", Json.String "RemoveStroke"), ("element", recordToJson s)]
              NoOpServer     -> [("action", Json.String "NoOpServer"), ("element", Json.Null)]
              NewClient      -> [("action", Json.String "NewClient"), ("element", Json.Null), ("board", recordToJson binfo)]
              NewDrawing     -> [("action", Json.String "NewDrawing"), ("element", Json.Null)]
        in toJsonObj <| ("drawing", recordToJson d) :: jav
    Nothing -> toJsonObj <| [("action", Json.String "NewClient"), ("element", Json.Null), ("drawing", Json.Null), ("board", recordToJson binfo)]


toJsonObj : [(String, Json.Value)] -> Json.Value
toJsonObj ps = Json.Object <| Dict.fromList ps







