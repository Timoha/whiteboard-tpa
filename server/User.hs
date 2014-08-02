{-# LANGUAGE OverloadedStrings #-}

module User (User (..)) where

import Data.Aeson
import Control.Applicative
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data User = User { firstName :: T.Text
                 , lastName :: T.Text
                 , email :: T.Text
                 } deriving Show



instance FromJSON User where
    parseJSON (Object v) = User <$>
                           v .: "firstName" <*>
                           v .: "lastName" <*>
                           v .: "email"
    parseJSON _          = mzero


