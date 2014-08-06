{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Drawing
where


import User

import Data.List
import Data.Maybe
import Data.Time
import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Aeson
import Data.SafeCopy
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


type DrawingId = Int
type StrokeId = Int
type BoardId = Int


--------------- Drawing ------------------


data Drawing = Drawing
    { drawingId :: DrawingId
    , boardId :: BoardId
    , strokes :: Maybe [Stroke]
    , user :: User
    , created :: UTCTime
    , submitted :: Maybe UTCTime
    } deriving Show


instance FromRow Drawing where
    fromRow = Drawing <$> field <*> field <*> field <*> (User <$> field <*> field <*> field) <*> field <*> field



--------------- DrawingInfo ------------------

data DrawingInfo = DrawingInfo DrawingId T.Text T.Text (Maybe [Stroke]) deriving (Show, Typeable)



data DrawingsDelete = DrawingsDelete [DrawingId]

instance FromJSON DrawingsDelete where
  parseJSON (Object v) = DrawingsDelete <$>
                         v .: "drawingIds"
  parseJSON _          = mzero

instance FromJSON DrawingInfo where
    parseJSON (Object v) = DrawingInfo <$>
                           v .: "drawingId" <*>
                           v .: "firstName" <*>
                           v .: "lastName" <*>
                           v .:? "strokes"
    parseJSON _          = mzero



instance ToJSON DrawingInfo where
    toJSON (DrawingInfo drawingId firstName lastName ss) =
        object [ "drawingId" .= drawingId
               , "firstName" .= firstName
               , "lastName" .= lastName
               , "strokes" .= ss ]


instance SafeCopy DrawingInfo where
     putCopy (DrawingInfo did fstN lstN ss) = contain $ do safePut did; safePut fstN; safePut lstN; safePut ss
     getCopy = contain $ DrawingInfo <$> safeGet <*> safeGet <*> safeGet <*> safeGet


toDrawingInfo :: Drawing -> DrawingInfo
toDrawingInfo (Drawing did _ ss (User fn ln e) _ _) = DrawingInfo did fn ln ss


--------------- Point ------------------

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Typeable, Eq, Show)

instance FromJSON Point where
    parseJSON (Object v) = Point <$>
                           v .: "x" <*>
                           v .: "y"
    parseJSON _          = mzero


instance ToJSON Point where
    toJSON (Point x y) =
        object [ "x" .= x
               , "y" .= y ]

instance SafeCopy Point where
     putCopy (Point x y) = contain $ do safePut x; safePut y
     getCopy = contain $ Point <$> safeGet <*> safeGet
--------------- Color ------------------

data Color =  Color
    { red :: Int
    , green :: Int
    , blue :: Int
    , alpha :: Float
    } deriving (Typeable, Eq, Show)


instance FromJSON Color where
    parseJSON (Object v) = Color <$>
                           v .: "red" <*>
                           v .: "green" <*>
                           v .: "blue" <*>
                           v .: "alpha"
    parseJSON _          = mzero


instance ToJSON Color where
    toJSON (Color r g b a) =
        object [ "red" .= r
               , "green" .= g
               , "blue" .= b
               , "alpha" .= a ]


instance FromField Color where
    fromField = fromJSONField


instance ToField Color where
    toField = toJSONField

instance SafeCopy Color where
     putCopy (Color r g b a) = contain $ do safePut r; safePut g; safePut b; safePut a;
     getCopy = contain $ Color <$> safeGet <*> safeGet <*> safeGet <*> safeGet


--------------- Brush ------------------

data Brush = Brush
    { color :: Color
    , size :: Int
    } deriving (Eq, Show)


instance FromJSON Brush where
    parseJSON (Object v) = Brush <$>
                           v .: "color" <*>
                           v .: "size"
    parseJSON _          = mzero


instance ToJSON Brush where
    toJSON (Brush c s) =
        object [ "color" .= c
               , "size" .= s ]

instance SafeCopy Brush where
     putCopy (Brush c s) = contain $ do safePut c; safePut s;
     getCopy = contain $ Brush <$> safeGet <*> safeGet

--------------- Stroke ------------------

data Stroke = Stroke
    { strokeId :: StrokeId
    , t0 :: Int
    , points :: [Point]
    , brush :: Brush
    } deriving (Eq, Show, Typeable)


instance Ord Stroke where
    (Stroke t01 _ _ _) `compare` (Stroke t02 _ _ _) = t01 `compare` t02


instance FromJSON Stroke where
    parseJSON (Object v) = Stroke <$>
                           v .: "id" <*>
                           v .: "t0" <*>
                           v .: "points" <*>
                           v .: "brush"
    parseJSON _          = mzero


instance ToJSON Stroke where
    toJSON (Stroke sid t0 ps b) =
        object [ "id" .= sid
               , "t0" .= t0
               , "points" .= ps
               , "brush" .= b ]

instance SafeCopy Stroke where
     putCopy (Stroke sid t0 ps b) = contain $ do safePut sid; safePut t0; safePut ps; safePut b;
     getCopy = contain $ Stroke <$> safeGet <*> safeGet <*> safeGet <*> safeGet


instance ToField [Stroke] where
    toField = toJSONField


instance FromField [Stroke] where
    fromField = fromJSONField


sortStrokes :: [Drawing] -> [Stroke]
sortStrokes ds = sort $ foldl (\ss d -> (getStrokes d) ++ ss) [] ds
    where
        getStrokes d = case strokes d of
            Just ss -> ss
            Nothing -> []


--------------- Database ------------------

create :: Connection -> User -> BoardId -> IO (Maybe Drawing)
create c (User firstName lastName email) bid =
    let q  = "insert into drawing (board_id, first_name, last_name, email, created) values (?, ?, ?, ?, NOW()) RETURNING *"
        vs = (bid, firstName, lastName, email)
    in fmap listToMaybe (query c q vs)


getSubmittedByBoard :: Connection -> BoardId -> IO [Drawing]
getSubmittedByBoard c bid =
    let q = "select * from drawing where board_id = ? and strokes is not null and submitted is not null"
        vs = (Only bid)
    in query c q vs


countAllByBoard :: Connection -> BoardId -> IO Int
countAllByBoard c bid =
    let q = "select * from drawing where board_id = ?"
        vs = (Only bid)
    in fmap length ((query c q vs) :: IO [Drawing])


submit :: Connection -> [Stroke] -> DrawingId -> IO (Maybe Drawing)
submit c ss did =
    let q  = "update drawing set strokes = ?, submitted = NOW() where drawing_id = ? RETURNING *"
        vs = (ss, did)
    in fmap listToMaybe (query c q vs)


deleteByIds :: Connection -> [DrawingId] -> BoardId -> IO [Drawing]
deleteByIds c dids bid =
    let q  = "delete from drawing where board_id = ? and drawing_id in ? RETURNING *"
        vs = (bid, In dids)
    in query c q vs
