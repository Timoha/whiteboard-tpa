{-# LANGUAGE OverloadedStrings #-}

module ServerError where

import Network.HTTP.Types as HttpType
import Data.Aeson (ToJSON, toJSON, object, (.=), encode)

import Data.Text
import Data.Text.Encoding

data ServerError = ServerError Text Status

instance ToJSON ServerError where
    toJSON (ServerError error status) =
        object [ "error" .= error
               , "status" .= status ]


instance ToJSON HttpType.Status where
    toJSON (HttpType.Status statusCode statusMessage) =
        object [ "code" .= statusCode
               , "message" .= decodeUtf8 statusMessage ]

tooManyPainters = HttpType.Status 507 "Too many painters"