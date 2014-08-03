{-# LANGUAGE OverloadedStrings #-}

module Api (ServerError (..), apiApp) where

import User
import DbConnect
import WixInstance
import qualified Drawing as Drawing
import qualified Board as Board

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BLC8

import Web.Scotty
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Network.HTTP.Types as HttpType


data ServerError = ServerError { error :: T.Text
                               , status :: HttpType.Status
                               } deriving Show


data WidgetJson = WidgetJson Board.Board [Drawing.Stroke] deriving Show

data SettingsPanelJson = SettingsPanelJson Board.Board Bool deriving Show


instance ToJSON WidgetJson where
    toJSON (WidgetJson ss ds) =
        object [ "settings" .= ss
               , "drawings" .= ds ]

instance ToJSON SettingsPanelJson where
    toJSON (SettingsPanelJson ss empty) =
        object [ "settings" .= ss
               , "empty" .= empty ]


instance ToJSON ServerError where
    toJSON (ServerError error status) =
        object [ "error" .= error
               , "status" .= status ]


instance ToJSON HttpType.Status where
    toJSON (HttpType.Status statusCode statusMessage) =
        object [ "code" .= statusCode
               , "message" .= (T.decodeUtf8 statusMessage) ]






getWixWidget :: ActionM (Maybe Board.WixWidget)
getWixWidget = do
    componentId <- param "compId"
    wixInstance <- header "X-Wix-Instance"
    let wixInstance' = fmap TL.encodeUtf8 wixInstance
    let widget = wixInstance' >>= parseInstance (BLC8.pack "e5e44072-34f2-43cc-ba7d-82962348d57f")
    return $ fmap (Board.WixWidget componentId) widget


apiApp :: ScottyM ()
apiApp = do

    middleware $ staticPolicy (noDots >-> addBase "public-dev")
    middleware logStdoutDev



    get "/" $ do
        widget <- getWixWidget
        case widget of
            Just w -> text $ TL.pack $ show w
            Nothing -> json $ (TL.decodeUtf8 . encode) $ ServerError "invalid wix instance" HttpType.badRequest400


    post "/api/board/:bid/drawing" $ do
        bid <- param "bid"
        b <- jsonData
        liftIO $ putStrLn $ show b
        case b of
          Just usr@(User _ _ _) -> do
              cdb <- liftIO $ connect dbConnectInfo
              d   <- liftIO $ Drawing.create cdb usr bid
              case d of
                  Just drawing -> json $ (TL.decodeUtf8 . encode . Drawing.toDrawingInfo) drawing
                  Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot create drawing" HttpType.internalServerError500)
          _ -> json $ (TL.decodeUtf8 . encode) (ServerError "invalid message format" HttpType.badRequest400)


    post "/api/drawing/:did/submit" $ do
        did <- param "did"
        ss  <- jsonData
        liftIO $ putStrLn $ show ss
        cdb <- liftIO $ connect dbConnectInfo
        d   <- liftIO $ Drawing.submit cdb ss did
        case d of
            Just _  -> json $ (TL.decodeUtf8 . encode) HttpType.ok200
            Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot find drawing" HttpType.badRequest400)


    get "/api/board/:compId" $ do
        widget <- getWixWidget
        cdb   <- liftIO $ connect dbConnectInfo
        board <- liftIO $ case widget of
            Just w -> Board.getOrCreate cdb Board.defaultSettings w
            Nothing -> return Nothing

        liftIO $ putStrLn $ show board
        liftIO $ putStrLn $ show widget
        case board of
            Just b -> do
                drawing <- liftIO $ Drawing.getSubmittedByBoard cdb (Board.boardId b)
                json $ (TL.decodeUtf8 . encode) $ WidgetJson b (Drawing.sortStrokes drawing)
            Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot get board" HttpType.internalServerError500)


    get "/api/board/:compId/settings" $ do
        widget <- getWixWidget
        cdb   <- liftIO $ connect dbConnectInfo
        board <- liftIO $ case widget of
            Just w -> Board.get cdb w
            Nothing -> return Nothing

        liftIO $ putStrLn $ show board
        liftIO $ putStrLn $ show widget
        case board of
            Just b -> do
                num <- liftIO $ Drawing.countAllByBoard cdb (Board.boardId b)
                json $ (TL.decodeUtf8 . encode) $ SettingsPanelJson b (num == 0)
            Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot get board" HttpType.internalServerError500)


    put "/api/board/:compId/settings" $ do
        widget <- getWixWidget
        bs  <- jsonData
        cdb   <- liftIO $ connect dbConnectInfo
        board <- liftIO $ case widget of
            Just w -> Board.update cdb bs w
            Nothing -> return Nothing
        case board of
            Just _  -> json $ (TL.decodeUtf8 . encode) HttpType.ok200
            Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot update drawing" HttpType.internalServerError500)

    --get "/api/board/:compId/settings" $ do
    --    widget <- getWixWidget
    --    cdb <- liftIO $ connect dbConnectInfo
    --    let board = widget >>= Board.update cdb
    --    let resp = case board of
    --        Nothing -> widget >>= Board.create cdb Board.defaultSettings
    --        b       -> b
    --    case resp of
    --        Just b  -> json $ (TL.decodeUtf8 . encode) b
    --        Nothing -> json $ (TL.decodeUtf8 . encode) (ServerError "cannot get board" HttpType.internalServerError500)


    

