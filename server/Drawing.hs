{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Drawing
where


import User

import Data.Maybe
import Data.Time
import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


type DrawingId = Int


type BoardId = Int


--------------- Drawing ------------------


data Drawing = Drawing
    { drawingId :: DrawingId
    , boardId :: BoardId
    , strokes :: Maybe T.Text
    , user :: User
    , created :: UTCTime
    , submitted :: Maybe UTCTime
    } deriving Show


instance FromRow Drawing where
    fromRow = Drawing <$> field <*> field <*> field <*> (User <$> field <*> field <*> field) <*> field <*> field



--------------- DrawingInfo ------------------

data DrawingInfo = DrawingInfo DrawingId T.Text T.Text deriving Show


instance FromJSON DrawingInfo where
    parseJSON (Object v) = DrawingInfo <$>
                           v .: "drawingId" <*>
                           v .: "firstName" <*>
                           v .: "lastName"
    parseJSON _          = mzero



instance ToJSON DrawingInfo where
    toJSON (DrawingInfo drawingId firstName lastName) =
        object [ "drawingId" .= drawingId
               , "firstName" .= firstName
               , "lastName" .= lastName ]



toDrawingInfo :: Drawing -> DrawingInfo
toDrawingInfo (Drawing did _ _ (User fn ln e) _ _) = DrawingInfo did fn ln




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



--------------- Stroke ------------------

data Stroke = Stroke
    { t0 :: Int
    , points :: [Object]
    , brush :: Brush
    } deriving (Eq, Show)


instance Ord Stroke where
    (Stroke t01 _ _) `compare` (Stroke t02 _ _) = t01 `compare` t02


instance FromJSON Stroke where
    parseJSON (Object v) = Stroke <$>
                           v .: "t0" <*>
                           v .: "points" <*>
                           v .: "brush"
    parseJSON _          = mzero


instance ToJSON Stroke where
    toJSON (Stroke t0 ps b) =
        object [ "t0" .= t0
               , "points" .= ps
               , "brush" .= b ]


instance ToField [Stroke] where
    toField = toJSONField




--------------- Database ------------------

create :: Connection -> User -> BoardId -> IO (Maybe Drawing)
create c (User firstName lastName email) bid =
    let q  = "insert into drawing (board_id, first_name, last_name, email, created) values (?, ?, ?, ?, NOW()) RETURNING *"
        vs = (bid, firstName, lastName, email)
    in fmap listToMaybe (query c q vs)


submit :: Connection -> [Stroke] -> DrawingId -> IO (Maybe Drawing)
submit c ss did =
    let q  = "update drawing set strokes = ?, submitted = NOW() where drawing_id = ? RETURNING *"
        vs = (ss, did)
    in fmap listToMaybe (query c q vs)