{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE DeriveGeneric #-}
--import GHC.Generics


module Realtime (application, defaultServerState) where

import Drawing
import DrawingProgress
import WixInstance
import Api (ServerError (..))
import Board


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
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.HashMap.Strict as HashMap

import Data.Acid as Acid
import Data.Aeson
import Data.Aeson.Types
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.HTTP.Types as HttpType




--------------- ClientMessage ------------------

data ClientMessage = ClientMessage
    { board :: BoardInfoUnparsed
    , drawing :: Maybe DrawingInfo
    , element :: Value
    , action :: T.Text
    } deriving Show




instance ToJSON ClientMessage where
    toJSON (ClientMessage board drawing element action) =
        object [ "drawing" .= drawing
               , "element" .= element
               , "action" .= action
               , "board" .=  board ]




instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage <$>
                           v .: "board" <*>
                           v .: "drawing" <*>
                           v .: "element" <*>
                           v .: "action"
    parseJSON _          = mzero




--instance FromJSON AddStrokes where
--    parseJSON (Object v) = AddStrokes <$>
--                           v .: "strokes"
--    parseJSON _          = mzero


instance FromJSON ReceivedPoint where
    parseJSON (Object v) = ReceivedPoint <$> v .: "t0" <*> v .: "id" <*> (Point <$> v .: "x" <*> v .: "y")
    parseJSON _          = mzero

data Message = NewClient BoardId
             | AddPoints DrawingInfo Brush [ReceivedPoint]
             | AddStrokes DrawingInfo [Stroke]
             | RemoveStroke DrawingInfo StrokeId
             | NoOp
             deriving (Show)




type Client = Int
type ClientState = HashMap.HashMap Client WS.Connection
type ServerState = HashMap.HashMap BoardId ClientState



defaultServerState :: ServerState
defaultServerState = HashMap.empty

defaultClientState :: ClientState
defaultClientState = HashMap.empty



--numClients :: ServerState -> Int
--numClients = HashMap.foldl' ((+) HashMap.size) 0


addClient :: BoardId -> Client -> WS.Connection -> ServerState -> ServerState
addClient b c w bs = HashMap.insert b clients bs
    where
        clients = case HashMap.lookup b bs of
            Just cs -> HashMap.insert c w cs
            Nothing -> HashMap.insert c w defaultClientState



removeClient :: BoardId -> Client -> ServerState -> ServerState
removeClient b c = HashMap.adjust (HashMap.delete c) b


broadcast :: TL.Text -> ClientState -> IO ()
broadcast message clients = do
    TL.putStrLn message
    forM_ (HashMap.elems clients) $ \c -> forkIO $ WS.sendTextData c message


broadcastBoard :: BoardId -> TL.Text -> (TL.Text -> ClientState -> IO ()) -> ServerState -> IO ()
broadcastBoard b m f bs =
    case HashMap.lookup b bs of
        Just cs -> f m cs
        Nothing -> putStrLn "Couldn't find board"


broadcastOther :: Client -> TL.Text -> ClientState -> IO ()
broadcastOther me message clients = broadcast message $ HashMap.filterWithKey (\k _ -> k /= me) clients



handleMessage :: ClientMessage -> Maybe Message
handleMessage msg =
    case action msg of
        "NewClient"  -> wixInst >>= (\w -> Just $ NewClient bid) -- check wix instance only for new client, after that it doesn't matter what she sends
        "AddPoints"  -> d >>= \d' -> flip parseMaybe e $ \obj -> AddPoints d' <$> obj .: "brush" <*> obj .: "points"
        "AddStrokes" -> d >>= \d' -> flip parseMaybe e $ \obj -> AddStrokes d' <$> obj .: "strokes"
        "RemoveStroke" -> d >>= \d' -> flip parseMaybe e $ \obj -> RemoveStroke d' <$> obj .: "strokeId"
        "NoOpServer" -> Just NoOp
        _ -> Nothing
    where
        (BoardInfoUnparsed bid inst cid) = board msg
        wixInst = parseInstance (BLC8.pack "e5e44072-34f2-43cc-ba7d-82962348d57f") (TL.encodeUtf8 (TL.fromStrict inst))
        d = drawing msg
        Object e = element msg




application :: MVar ServerState -> Acid.AcidState BoardsState -> WS.ServerApp
application state acid pending = do
    clientUnique <- newUnique
    let client = hashUnique clientUnique
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn
    putStrLn $ show $ msg
    boards <- liftIO $ readMVar state
    case decode msg of
        Just m -> case handleMessage m of
            Just (NewClient bid) -> do
                broadcastBoard bid "new client joined" broadcast boards
                online <- liftIO $ Acid.query acid (GetDrawings bid)
                WS.sendTextData conn $ encode (ClientMessage (board m) (drawing m) (object ["online" .= online]) "AddDrawings")
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = addClient bid client conn s
                    return s'
                talk conn state acid bid client
            _           -> return ()
        Nothing -> do (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)



talk :: WS.Connection -> MVar ServerState -> Acid.AcidState BoardsState -> BoardId -> Client -> IO ()
talk conn state acid bid client = handle catchDisconnect $
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Just m@(ClientMessage _ _ _ _)  -> case handleMessage m of
                Just (AddPoints d b ps) -> do
                    liftIO $ Acid.update acid $ AddNewPoints bid d (map (applyBrush b) ps)
                    liftIO $ readMVar state >>= broadcastBoard bid (TL.decodeUtf8 msg) (broadcastOther client)
                Just (AddStrokes d ss) -> do
                    liftIO $ Acid.update acid $ AddNewStrokes bid d ss
                    liftIO $ readMVar state >>= broadcastBoard bid (TL.decodeUtf8 msg) (broadcastOther client)
                Just s@(RemoveStroke d sid) -> do
                    liftIO $ Acid.update acid $ RemoveOldStroke bid d sid
                    liftIO $ readMVar state >>= broadcastBoard bid (TL.decodeUtf8 msg) (broadcastOther client)
                _ -> liftIO $ readMVar state >>= broadcastBoard bid (TL.decodeUtf8 msg) (broadcastOther client)
            _ -> do (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO disconnectClient
                _ -> liftIO disconnectClient
            where
                disconnectClient = modifyMVar_ state $ \s -> do
                    putStrLn $ show e
                    let s' = removeClient bid client s
                    broadcastBoard bid "disconnected" broadcast s'
                    return s'