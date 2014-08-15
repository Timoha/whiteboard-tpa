{-# LANGUAGE OverloadedStrings #-}

module ServerError where

import Network.HTTP.Types as HttpType
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)

import Data.Text
import Data.Text.Encoding

import qualified Data.Text.Lazy.Encoding as TL

import Web.Scotty.Trans

import qualified Data.ByteString.Lazy as BL


instance ToJSON ServerError where
    toJSON (ServerError error status) =
        object [ "error" .= error
               , "status" .= status ]


instance ToJSON HttpType.Status where
    toJSON (HttpType.Status statusCode statusMessage) =
        object [ "code" .= statusCode
               , "message" .= decodeUtf8 statusMessage ]


data ServerError = ServerError Text Status deriving (Show, Eq)

instance ScottyError ServerError where
    stringError = \x -> ServerError (pack x) HttpType.internalServerError500
    showError = TL.decodeUtf8 . encode


handleEx :: Monad m => ServerError -> ActionT ServerError m ()
handleEx srvError@(ServerError _ statusCode)  = do
    status statusCode
    json srvError


tooManyPainters = HttpType.Status 507 "Too many painters"