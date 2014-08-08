{-# LANGUAGE OverloadedStrings #-}

module Board where

import Drawing (Color (..), BoardId)
import WixInstance

import Data.Time
import Data.List
import Data.List.Split
import Data.Monoid (mappend)
import Data.Maybe

import Control.Monad
import Control.Applicative

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--------------- Paper ------------------

propIsoA :: Double
propIsoA = sqrt 2

propAnsi :: Double
propAnsi = 17 / 11

-- 1px = 0.3mm
pixelToMm :: Double
pixelToMm = 0.3

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
        "ANSI_A" -> return ANSI_A
        "ANSI_B" -> return ANSI_B
        "ANSI_C" -> return ANSI_C
        "ANSI_D" -> return ANSI_D
        "ANSI_E" -> return ANSI_E
        "ISO_A4" -> return ISO_A4
        "ISO_A3" -> return ISO_A3
        "ISO_A2" -> return ISO_A2
        "ISO_A1" -> return ISO_A1
        "ISO_A0" -> return ISO_A0
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
            let width  = if "ANSI" `isPrefixOf` show t
                            then (w::Double) * propAnsi / pixelToMm
                            else (w::Double) * propIsoA / pixelToMm
                height = w / pixelToMm
            in Dimensions (round width) (round height) t



data Dimensions = Dimensions
    { width :: Int
    , height :: Int
    , paper :: Paper
    } deriving (Show)


getDimensions :: Paper -> [Dimensions] -> Maybe Dimensions
getDimensions pt = find (\x -> paper x == pt)



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
    , design :: Value
    } deriving (Show)


data BoardInfo = BoardInfo
    { widget :: WixWidget
    , whiteboard :: BoardId
    }

data BoardInfoUnparsed = BoardInfoUnparsed BoardId T.Text ComponentId deriving (Show)



defaultSettings :: BoardSettings
defaultSettings = BoardSettings (T.pack "Untitled")
                                ANSI_B
                                False
                                Nothing
                                (Color 255 255 255 1.0)
                                (object [])


instance FromJSON BoardSettings where
    parseJSON (Object v) = BoardSettings <$>
                           v .: "boardName" <*>
                           v .: "paperType" <*>
                           v .: "locked" <*>
                           v .:? "backgroundPicture" <*>
                           v .: "backgroundColor" <*>
                           v .: "design"
    parseJSON _          = mzero


instance FromJSON BoardInfoUnparsed where
    parseJSON (Object v) = BoardInfoUnparsed <$>
                           v .: "boardId" <*>
                           v .: "instance" <*>
                           v .: "componentId"
    parseJSON _          = mzero


instance ToJSON BoardInfoUnparsed where
    toJSON (BoardInfoUnparsed bid inst cid) =
        object [ "boardId" .= bid
               , "instance" .= inst
               , "componentId" .= cid ]

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

data WixWidget = WixWidget
    { componentId :: ComponentId
    , wixInstance :: WixInstance
    } deriving Show

type ComponentId = T.Text

data WidgetId = WidgetId T.Text T.Text Int

instance FromRow WidgetId where
    fromRow = WidgetId <$>
              field <*>
              field <*>
              field



create :: Connection -> BoardSettings -> WixWidget -> IO (Maybe Board)
create c bs (WixWidget componentId (WixInstance instanceId _)) =
    let q  = "insert into board (instance_id, name, paper_size, locked, background_picture, background_color, design, created)"
              `mappend` " values (?, ?, ?, ?, ?, ?, ?, NOW()) RETURNING *"
        vs = (instanceId, boardName bs, paperType bs, locked bs, backgroundPicture bs, backgroundColor bs, design bs)
    in listToMaybe <$> query c q vs


update :: Connection -> BoardSettings -> WixWidget -> IO (Maybe Board)
update c bs (WixWidget componentId (WixInstance instanceId _)) =
    let q  = "update board as b set name = ?, paper_size = ?, locked = ?, background_picture = ?, background_color = ?, design = ?"
              `mappend` "from wix_widget as w where b.board_id = w.board_id RETURNING b.*"
        vs = (boardName bs, paperType bs, locked bs, backgroundPicture bs, backgroundColor bs, design bs)
    in listToMaybe <$> query c q vs


get :: Connection -> WixWidget -> IO (Maybe Board)
get c (WixWidget componentId (WixInstance instanceId _)) =
    let q  = "select b.board_id, b.instance_id, name, paper_size, locked, background_picture, background_color, design, created " `mappend`
             "from wix_widget as w, board as b " `mappend`
             "where w.instance_id = ? and w.component_id = ? and b.board_id = w.board_id"
        vs = (instanceId, componentId)
    in listToMaybe <$> query c q vs



getByInstance :: Connection -> WixWidget -> IO [Board]
getByInstance c (WixWidget componentId (WixInstance instanceId _)) =
    let q  = "select * "   `mappend`
             "from board " `mappend`
             "where instance_id = ?"
        vs = Only instanceId
    in query c q vs


getOrCreate :: Connection -> BoardSettings -> WixWidget -> IO (Maybe Board)
getOrCreate c bs w = do
    boardFromDb <- get c w
    case boardFromDb of
        Nothing -> do
          newBoard <- create c bs w
          case newBoard of
              Just b  -> do
                  createWixWidget c b w
                  return newBoard
              Nothing -> return Nothing
        b -> return b

createWixWidget :: Connection -> Board -> WixWidget -> IO (Maybe WidgetId)
createWixWidget c b (WixWidget componentId (WixInstance instanceId _)) =
    let q  = "insert into wix_widget (instance_id, component_id, board_id)"
              `mappend` " values (?, ?, ?) RETURNING *"
        vs = (instanceId, componentId, boardId b)
    in listToMaybe <$> query c q vs
