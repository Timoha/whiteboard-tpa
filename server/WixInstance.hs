{-# LANGUAGE OverloadedStrings #-}

module WixInstance
where

import Control.Monad
import Control.Applicative

import Data.Text
import Data.Aeson
import Crypto.MAC.HMAC
import qualified Crypto.Hash.SHA256 as SHA256

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.ByteString.Base64.URL.Lazy as Base64Url


type InstanceId = Text
data Permissions = Owner | None  deriving (Show)


data WixInstance = WixInstance
    { instanceId :: InstanceId
    , permissions :: Permissions
    } deriving (Show)


instance FromJSON Permissions where
    parseJSON (String "OWNER") = return Owner
    parseJSON Null             = return None
    parseJSON _                = mzero


instance FromJSON WixInstance where
    parseJSON (Object v) = WixInstance <$>
                           v .: "instanceId" <*>
                           v .: "permissions"
    parseJSON _          = mzero



isValidInstance :: BL.ByteString -> BL.ByteString -> BL.ByteString -> Bool
isValidInstance secret signature inst = newS == s where
    s = BL.toStrict $ Base64Url.decodeLenient signature
    newS = hmac SHA256.hash 64 (BL.toStrict secret) (BL.toStrict inst)



splitSignedInstance :: BL.ByteString -> Maybe (BL.ByteString, BL.ByteString)
splitSignedInstance signed =
    case BLC8.split '.' signed of
        ("": _)  -> Nothing
        (_:[])   -> Nothing
        (s:i:[]) -> Just (s, i)
        (_:_:_)  -> Nothing



parseInstance :: BL.ByteString -> BL.ByteString -> Maybe WixInstance
parseInstance secret si = do
    (s, i) <- splitSignedInstance si
    if isValidInstance secret s i
        then decode (Base64Url.decodeLenient i) :: Maybe WixInstance
        else Nothing

