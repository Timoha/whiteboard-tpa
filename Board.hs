{-# LANGUAGE OverloadedStrings #-}

module Board where

import Drawing (Color (..), BoardId)

import Data.Time
import Data.List
import Data.List.Split
import Data.Monoid (mappend)

import Control.Applicative
import Control.Monad

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--------------- Paper ------------------

prop_iso_a :: Double
prop_iso_a = sqrt 2

prop_ansi :: Double
prop_ansi = 17 / 11

-- 1px = 0.3mm
pixel_to_mm :: Double
pixel_to_mm = 0.3

data Paper
    = ANSI_A
    | ANSI_B
    | ANSI_C
    | ANSI_D
    | ANSI_E
    | ISO_A4
    | ISO_A3
    | ISO_A2
    | ISO_A1
    | ISO_A0
    deriving (Show, Read, Enum, Eq)

instance FromJSON Paper where
    parseJSON (String s) = case s of
        "ANSI_A" -> pure ANSI_A
        "ANSI_B" -> pure ANSI_B
        "ANSI_C" -> pure ANSI_C
        "ANSI_D" -> pure ANSI_D
        "ANSI_E" -> pure ANSI_E
        "ISO_A4" -> pure ISO_A4
        "ISO_A3" -> pure ISO_A3
        "ISO_A2" -> pure ISO_A2
        "ISO_A1" -> pure ISO_A1
        "ISO_A0" -> pure ISO_A0
        _        -> mzero
    parseJSON _ = mzero


instance ToJSON Paper where
    toJSON p = String $ T.pack $ show p


instance FromField Paper where
    fromField f p = fmap (\x -> read (T.unpack x) :: Paper) (fromField f p)


instance ToField Paper where
    toField p = toField (show p)

paperDims :: [Dimensions]
paperDims = map toDims
    [ (ANSI_A, 216)
    , (ANSI_B, 279)
    , (ANSI_C, 432)
    , (ANSI_D, 559)
    , (ANSI_E, 864)
    , (ISO_A4, 210)
    , (ISO_A3, 297)
    , (ISO_A2, 420)
    , (ISO_A1, 594)
    , (ISO_A0, 841) ]
    where
        toDims (t, w) =
            let height = if ("ANSI" `isPrefixOf` (show t))
                            then (w::Double) * prop_ansi / pixel_to_mm
                            else (w::Double) * prop_iso_a / pixel_to_mm
            in Dimensions (round w) (round height) t



data Dimensions = Dimensions
    { width :: Int
    , height :: Int
    , paper :: Paper
    } deriving (Show)


getDimensions :: Paper -> [Dimensions] -> Maybe Dimensions
getDimensions pt xs = find (\x -> (paper x) == pt) xs



instance ToJSON Dimensions where
    toJSON (Dimensions w h p) =
        object [ "width" .= w
               , "height" .= h
               , "paperType" .= p]



--------------- Board ------------------


data Board = Board
    { boardId :: BoardId
    , userId :: InstanceId
    , settings :: BoardSettings
    , created :: UTCTime
    } deriving (Show)


data BoardSettings = BoardSettings
    { boardName :: T.Text
    , paperType :: Paper
    , locked :: Bool
    , backgroundPicture :: Maybe T.Text
    , backgroundColor :: Color
    , design :: T.Text
    } deriving (Show)


instance FromJSON BoardSettings where
    parseJSON (Object v) = BoardSettings <$>
                           v .: "boardName" <*>
                           v .: "paperType" <*>
                           v .: "locked" <*>
                           v .:? "backgroundPicture" <*>
                           v .: "backgroundColor" <*>
                           v .: "design"
    parseJSON _          = mzero


instance ToJSON Board where
    toJSON (Board boardId _ (BoardSettings name paperType locked backgroundPicture backgroundColor design) _) =
        object [ "boardId" .= boardId
               , "boardName" .= name
               , "dimensions" .= getDimensions paperType paperDims
               , "locked" .= locked
               , "backgroundPicture" .= backgroundPicture
               , "backgroundColor" .= backgroundColor
               , "design" .= design ]


instance FromRow Board where
    fromRow = Board <$>
              field <*>
              field <*>
              (BoardSettings <$> field <*> field <*> field <*> field <*> field <*> field) <*>
              field

type InstanceId = T.Text
type ComponentId = T.Text

data WixWidget = WixWidget
    { instanceId :: InstanceId
    , componentId :: ComponentId
    }

create :: Connection -> WixWidget -> BoardSettings -> IO [Board]
create c (WixWidget instanceId componentId) bs =
    let q  = "insert into board (instance_id, name, paper_size, locked, background_picture, background_color, design, created)"
              `mappend` " values (?, ?, ?, ?, ?, ?, ?, NOW()) RETURNING *"
        vs = (instanceId, boardName bs, paperType bs, locked bs, backgroundPicture bs, backgroundColor bs, design bs)
    in query c q vs


update :: Connection -> WixWidget -> BoardId -> BoardSettings -> IO [Board]
update c (WixWidget instanceId componentId) bid bs =
    let q  = "update board set instance_id = ?, name = ?, paper_size = ?, locked = ?, background_picture = ?, background_color = ?, design = ?"
              `mappend` " where board_id = ? RETURNING *"
        vs = (instanceId, boardName bs, paperType bs, locked bs, backgroundPicture bs, backgroundColor bs, design bs, bid)
    in query c q vs


get :: Connection -> WixWidget -> IO [Board]
get c (WixWidget instanceId componentId) =
    let q  = "select b.board_id, w.instance_id, name, paper_size, locked, background_picture, background_color, design, created " `mappend`
             "from wix_widget as w, board as b " `mappend`
             "where instance_id = ? and component_id = ? and b.board_id = w.board_id"
        vs = (instanceId, componentId)
    in query c q vs


