module Api where

import Json
import Dict
import Canvas (Point, Brush, Stroke, WithId)
import JavaScript.Experimental as JS





type DrawingInfo =
  { firstName : String
  , lastName : String
  , drawingId : Int
  }


data ServerAction = AddPoints { brush:Brush, points:[WithId Point] }
                  | AddStrokes { strokes:[{id:Int, stroke:Stroke}] }
                  | RemoveStroke { strokeId:Int }
                  | NewClient
                  | NoOpServer





constructMessage : ServerAction -> Maybe DrawingInfo -> String
constructMessage a u = Json.toString "" <| jsonOfServerAction a u

jsonOfServerAction : ServerAction -> Maybe DrawingInfo -> Json.Value
jsonOfServerAction action info =
  case info of
    Just d ->
        let
          recordToJson = (JS.toJson . JS.fromRecord)
          jav = case action of
              AddPoints ps   -> [("action", Json.String "AddPoints"), ("element", recordToJson ps)]
              AddStrokes ss  -> [("action", Json.String "AddStrokes"), ("element", recordToJson ss)]
              RemoveStroke s -> [("action", Json.String "RemoveStroke"), ("element", recordToJson s)]
              NoOpServer     -> [("action", Json.String "NoOpServer"), ("element", Json.Null)]
              NewClient      -> [("action", Json.String "NewClient"), ("element", Json.Null)]
        in toJsonObj <| ("drawing", recordToJson d) :: jav
    Nothing -> toJsonObj <| [("action", Json.String "NewClient"), ("element", Json.Null), ("drawing", Json.Null)]



(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
ma >>= f = case ma of {
  Just a -> f a;
  Nothing -> Nothing }

toJsonObj : [(String, Json.Value)] -> Json.Value
toJsonObj ps = Json.Object <| Dict.fromList ps



jsonOfPoint : (Int, Point) -> Json.Value
jsonOfPoint (sid, {x, y}) = toJsonObj
  [ ("x", Json.Number (toFloat x))
  , ("y", Json.Number (toFloat y))
  , ("id", Json.Number (toFloat sid))
  ]



jsonOfBrush : Brush -> Json.Value
jsonOfBrush {size, color} =
  let jsonOfColor = toJsonObj <|
    [ ("red", Json.Number (toFloat color.red))
    , ("green", Json.Number (toFloat color.green))
    , ("blue", Json.Number (toFloat color.blue))
    , ("alpha", Json.Number color.alpha)
    ]
  in toJsonObj [ ("size", Json.Number size)
               , ("color", jsonOfColor)
               ]


jsonOfStroke : (Int, Stroke) -> Json.Value
jsonOfStroke (sid, {points, brush})  = toJsonObj
  [ ("points", Json.Array <| map (\p -> jsonOfPoint (sid, p)) points)
  , ("brush", jsonOfBrush brush)
  , ("id", Json.Number (toFloat sid))
  ]


jsonOfType : String -> Int -> Json.Value -> Json.Value
jsonOfType msgType userId elem = toJsonObj
  [ ("type", Json.String msgType)
  , ("userId", Json.Number (toFloat userId))
  , ("element", elem)
  ]


jsonOfAddPoints : Brush -> [(Int, Point)] -> Int -> Json.Value
jsonOfAddPoints brush ps uId = jsonOfType "add_point" uId <| toJsonObj
  [ ("points", Json.Array <| map jsonOfPoint ps)
  , ("brush", jsonOfBrush brush)
  ]



jsonOfAddStokes : [(Int, Stroke)] -> Int -> Json.Value
jsonOfAddStokes ss uId = jsonOfType "add_strokes" uId (Json.Array <| map jsonOfStroke ss)



jsonOfRemoveStroke : Int -> Int -> Json.Value
jsonOfRemoveStroke sId uId = jsonOfType "remove_stroke" uId <| Json.Number (toFloat sId)


extractDict : Json.Value -> Maybe (Dict.Dict comparable Json.Value)
extractDict jv = case jv of
  Json.Object d -> Just d
  _ -> Nothing

extractFloat : Json.Value -> Maybe Float
extractFloat s = case s of
  Json.Number n -> Just n
  _ -> Nothing


extractString : Json.Value -> Maybe String
extractString jv = case jv of
  Json.String s -> Just s
  _ -> Nothing


extractType : Json.Value -> Maybe String
extractType jv =
  (extractDict jv >>= Dict.get "type") >>= extractString


extractPoint : Json.Value -> Maybe (Int, Point)
extractPoint jv =
  extractDict jv >>= (\d ->
  (Dict.get "x" d >>= extractFloat) >>= (\x ->
  (Dict.get "y" d >>= extractFloat) >>= (\y ->
  (Dict.get "id" d >>= extractFloat) >>= (\sid ->
  Just (round sid, { x = round x, y = round y }) ))))





extractColor jv =
  extractDict jv >>= (\d ->
  (Dict.get "red" d >>= extractFloat) >>= (\red ->
  (Dict.get "green" d >>= extractFloat) >>= (\green ->
  (Dict.get "blue" d >>= extractFloat) >>= (\blue ->
  (Dict.get "alpha" d >>= extractFloat) >>= (\alpha ->
  Just { red = round red, green = round green, blue = round blue, alpha = alpha } )))))

extractBrush : Json.Value -> Maybe Brush
extractBrush jv =
  extractDict jv >>= (\d ->
  (Dict.get "size" d >>= extractFloat) >>= (\size ->
  (Dict.get "color" d >>= extractColor) >>= (\color ->
  Just { size = size, color = color })))


extractAddPoints : Json.Value -> Maybe (Brush, [(Int, Point)])
extractAddPoints jv =
  extractDict jv >>= (\d ->
  (Dict.get "brush" d >>= extractBrush) >>= (\brush ->
  (case Dict.get "points" d of
    Just (Json.Array jps) -> Just . justs <| map extractPoint jps
    _                     -> Nothing ) >>= (\points ->
  Just (brush, points) )))


extractRemoveStroke : Json.Value -> Maybe Int
extractRemoveStroke jv =
  extractFloat jv >>= (\sId ->
  Just (round sId))

extractList : Json.Value -> [Json.Value]
extractList (Json.Array list) = list

extractStroke : Json.Value -> Maybe (Int, Stroke)
extractStroke jv =
  let withoutId p = case p of
    Just (sid, p) -> Just p
    _ -> Nothing
  in extractDict jv >>= (\d ->
  (Dict.get "id" d >>= extractFloat) >>= (\sid ->
  (Dict.get "brush" d >>= extractBrush) >>= (\brush ->
  (case Dict.get "points" d of
    Just (Json.Array jps) -> Just . justs <| map (withoutId . extractPoint) jps
    _                     -> Nothing ) >>= (\points ->
  Just (round sid, { brush = brush, points = points }) ))))


extractAddStrokes : Json.Value -> Maybe [(Int, Stroke)]
extractAddStrokes jv = case jv of
  Json.Array jss -> Just . justs <| map extractStroke jss
  _ -> Nothing
