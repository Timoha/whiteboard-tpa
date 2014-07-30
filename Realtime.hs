{-# LANGUAGE OverloadedStrings #-}

module Realtime (application, defaultServerState) where

import Drawing
import Api (ServerError (..))


import Data.Maybe
import Data.Unique
import Data.Monoid (mappend, mconcat)

import Control.Exception (fromException, handle)
import Control.Applicative
import Control.Monad
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.HTTP.Types as HttpType



--------------- ClientMessage ------------------

data ClientMessage = ClientMessage { drawing :: Maybe DrawingInfo
                                   , element :: Value
                                   , action :: T.Text
                                   } deriving Show

instance ToJSON ClientMessage where
    toJSON (ClientMessage drawing element action) =
        object [ "drawing" .= drawing
               , "element" .= element
               , "action" .= action ]


instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage <$>
                           v .: "drawing" <*>
                           v .: "element" <*>
                           v .: "action"
    parseJSON _          = mzero




data Message = NewClient
             | NewDrawing DrawingInfo
--             | RemoveStroke Stroke User
--             | AddPoints [Points] User
--             | AddStrokes [Stroke] User
             | NoOp




type Client = Int


type ServerState = HashMap.HashMap Client WS.Connection



defaultServerState :: ServerState
defaultServerState = HashMap.empty


numClients :: ServerState -> Int
numClients = HashMap.size


addClient :: Int -> WS.Connection -> ServerState -> ServerState
addClient = HashMap.insert


removeClient :: Int -> ServerState -> ServerState
removeClient = HashMap.delete


broadcast :: TL.Text -> ServerState -> IO ()
broadcast message clients = do
    TL.putStrLn message
    forM_ (HashMap.elems clients) $ \c -> forkIO $ WS.sendTextData c message


broadcastOther :: TL.Text -> Client -> ServerState -> IO ()
broadcastOther message me clients = broadcast message $ HashMap.filterWithKey (\k _ -> k /= me) clients




handleMessage :: ClientMessage -> Maybe Message
handleMessage msg =
    case action msg of
        "NewClient" -> Just NewClient
        "NewDrawing" -> fmap NewDrawing d
        "NoOpServer" -> Just NoOp
        _ -> Nothing
    where
        d = drawing msg




application :: MVar ServerState -> WS.ServerApp
application state pending = do
    putStrLn $ show $ WS.requestPath $ WS.pendingRequest pending
    clientUnique <- newUnique
    let client = hashUnique clientUnique
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn
    clients <- liftIO $ readMVar state
    case decode msg of
        Just m -> case handleMessage m of
            Just NewClient -> do
                broadcast "new client joined" clients
                WS.sendTextData conn ("hi new" :: T.Text)
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = addClient client conn s
                    putStrLn $ show $ numClients s'
                    return s'
                talk conn state client
            _           -> return ()
        Nothing -> do (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)



talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state client = handle catchDisconnect $
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Just (ClientMessage _ _ _)  -> liftIO $ readMVar state >>= broadcastOther (TL.decodeUtf8 msg) client
            _ -> do (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO disconnectClient
                _ -> liftIO disconnectClient
            where disconnectClient = modifyMVar_ state $ \s -> do
                    putStrLn $ show e
                    let s' = removeClient client s
                    broadcast "disconnected" s'
                    return s'