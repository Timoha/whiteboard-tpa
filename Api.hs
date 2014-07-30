{-# LANGUAGE OverloadedStrings #-}

module Api (ServerError (..), apiApp) where

import User
import DbConnect
import qualified Drawing as Drawing
import qualified Board as Board


import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL


import Web.Scotty
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Static
import qualified Network.HTTP.Types as HttpType


data ServerError = ServerError { error :: T.Text
                               , status :: HttpType.Status
                               } deriving Show


instance ToJSON ServerError where
    toJSON (ServerError error status) =
        object [ "error" .= error
               , "status" .= status ]


instance ToJSON HttpType.Status where
    toJSON (HttpType.Status statusCode statusMessage) =
        object [ "code" .= statusCode
               , "message" .= (T.decodeUtf8 statusMessage) ]



apiApp :: ScottyM ()
apiApp = do

    middleware $ staticPolicy (noDots >-> addBase "public-dev")

    get "/" $ file "index.html"


    post "/api/board/:bid/drawing" $ do
        bid <- param "bid"
        b <- jsonData
        liftIO $ putStrLn $ show b
        case b of
          Just usr@(User _ _ _) -> do
              cdb <- liftIO $ connect dbConnectInfo
              ds <- liftIO $ Drawing.create cdb usr bid
              if (not . null) ds
                  then json $ (TL.decodeUtf8 . encode . Drawing.toDrawingInfo) (head ds)
                  else json $ (TL.decodeUtf8 . encode) (ServerError "cannot create drawing" HttpType.internalServerError500)
          _ -> json $ (TL.decodeUtf8 . encode) (ServerError "invalid message format" HttpType.badRequest400)

    post "/api/drawing/:did/submit" $ do
        did <- param "did"
        b <- jsonData
        liftIO $ putStrLn $ show b
        cdb <- liftIO $ connect dbConnectInfo
        ds <- liftIO $ Drawing.submit cdb b did
        json $ (TL.decodeUtf8 . encode) HttpType.ok200


    -- get all styling settings with drawings, mode
    get "/api/board/:bid" $ text "board"

    -- get settings for settings panel - list of boards for this instance, no drawings
    get "/api/settings/:compId" $ text "sett get"

    -- save settings: board name, mode, printing size (i.e. ISO-A4), border-width
    put "/api/settings/:compId" $ text "sett put"
