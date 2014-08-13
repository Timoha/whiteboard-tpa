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
import DbConnect (dbConnectInfo)


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
import qualified Data.Map.Strict as Map

import Data.Acid as Acid
import Data.Aeson
import Data.Aeson.Types
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.HTTP.Types as HttpType
import Database.PostgreSQL.Simple


--------------- ClientMessage ------------------

data ClientMessage = ClientMessage
    { board :: Maybe BoardInfoUnparsed
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
                           v .:? "board" <*>
                           v .: "drawing" <*>
                           v .: "element" <*>
                           v .: "action"
    parseJSON _          = mzero




instance FromJSON ReceivedPoint where
    parseJSON (Object v) = ReceivedPoint <$> v .: "t0" <*> v .: "id" <*> (Point <$> v .: "x" <*> v .: "y")
    parseJSON _          = mzero

data Message = NewClient (Maybe DrawingInfo) BoardId
             | AddPoints DrawingInfo Brush [ReceivedPoint]
             | AddStrokes DrawingInfo [Stroke]
             | RemoveStroke DrawingInfo StrokeId
             | NewDrawing DrawingInfo
             | NoOp
             deriving (Show)




type Client = Int
data ClientInfo = ClientInfo WS.Connection (Maybe Drawing.DrawingInfo)
type ClientState = Map.Map Client ClientInfo
type ServerState = Map.Map BoardId ClientState



defaultServerState :: ServerState
defaultServerState = Map.empty

defaultClientState :: ClientState
defaultClientState = Map.empty



--numClients :: ServerState -> Int
--numClients = HashMap.foldl' ((+) HashMap.size) 0


addClient :: Client -> ClientInfo -> BoardId -> ServerState -> ServerState
addClient c info bid bs = Map.alter addC bid bs
    where addC board = case board of
                           Just b  -> Just $ Map.insert c info b
                           Nothing -> Just $ Map.insert c info defaultClientState


updateClient :: (ClientInfo -> ClientInfo) -> Client -> BoardId -> ServerState -> ServerState
updateClient f c = Map.adjust $ Map.adjust f c


lookupClient :: BoardId -> Client -> ServerState -> Maybe ClientInfo
lookupClient bid cid bs = Map.lookup bid bs >>= Map.lookup cid

setClientDrawing :: DrawingInfo -> ClientInfo -> ClientInfo
setClientDrawing d (ClientInfo conn _) = ClientInfo conn (Just d)



removeClient :: BoardId -> Client -> ServerState -> ServerState
removeClient bid c = Map.update rm bid
    where rm b = let removed = Map.delete c b
                 in if Map.null removed then Nothing else Just removed


broadcast :: TL.Text -> ClientState -> IO ()
broadcast message clients = do
    TL.putStrLn message
    forM_ (Map.elems clients) $ \(ClientInfo c _) -> forkIO $ WS.sendTextData c message


broadcastBoard :: BoardId -> TL.Text -> (TL.Text -> ClientState -> IO ()) -> ServerState -> IO ()
broadcastBoard b m f bs =
    case Map.lookup b bs of
        Just cs -> f m cs
        Nothing -> putStrLn "Couldn't find board"


broadcastOther :: Client -> TL.Text -> ClientState -> IO ()
broadcastOther me message clients = broadcast message $ Map.filterWithKey (\k _ -> k /= me) clients



verifyNewClient :: ClientMessage -> Maybe Message
verifyNewClient msg = do
    BoardInfoUnparsed bid inst _ <- board msg
    parseInstance (BLC8.pack "e5e44072-34f2-43cc-ba7d-82962348d57f") $ (TL.encodeUtf8 . TL.fromStrict) inst
    return $ NewClient (drawing msg) bid


handleMessage :: ClientMessage -> Maybe Message
handleMessage msg =
    case action msg of
        "NewClient"    -> verifyNewClient msg
        "AddPoints"    -> d >>= \d' -> flip parseMaybe e $ \obj -> AddPoints d' <$> obj .: "brush" <*> obj .: "points"
        "AddStrokes"   -> d >>= \d' -> flip parseMaybe e $ \obj -> AddStrokes d' <$> obj .: "strokes"
        "RemoveStroke" -> d >>= \d' -> flip parseMaybe e $ \obj -> RemoveStroke d' <$> obj .: "strokeId"
        "NoOpServer"   -> Just NoOp
        "NewDrawing"   -> d >>= Just . NewDrawing
        _ -> Nothing
    where
        d = drawing msg
        Object e = element msg




application :: MVar ServerState -> Acid.AcidState BoardsState -> WS.ServerApp
application state acid pending = do
    clientUnique <- newUnique
    let clientId = hashUnique clientUnique
    conn <- WS.acceptRequest pending
    msg <- WS.receiveData conn
    print msg
    boards <- liftIO $ readMVar state
    case decode msg of
        Just m -> case handleMessage m of
            Just (NewClient d bid) -> do
                liftIO $ modifyMVar_ state $ \s -> return $ addClient clientId (ClientInfo conn d) bid s
                talk conn state acid bid clientId
            _           -> return ()
        Nothing -> (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)



talk :: WS.Connection -> MVar ServerState -> Acid.AcidState BoardsState -> BoardId -> Client -> IO ()
talk conn state acid bid client = handle catchDisconnect $
    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Just m@ClientMessage{}  -> do
                liftIO $ readMVar state >>= broadcastBoard bid (TL.decodeUtf8 msg) (broadcastOther client)
                case handleMessage m of
                    Just (AddPoints d b ps)   -> liftIO $ Acid.update acid $ AddNewPoints bid d (map (applyBrush b) ps)
                    Just (AddStrokes d ss)    -> liftIO $ Acid.update acid $ AddNewStrokes bid d ss
                    Just (RemoveStroke d sid) -> liftIO $ Acid.update acid $ RemoveOldStroke bid d sid
                    Just (NewDrawing d)       -> liftIO $ modifyMVar_ state $ \s -> return $ updateClient (setClientDrawing d) client bid s
                    _ -> return ()
            _ -> (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO disconnectClient
                _ -> liftIO disconnectClient
            where
                disconnectClient = modifyMVar_ state $ \s -> do
                    print e
                    case lookupClient bid client s of
                        Just (ClientInfo _ (Just drawing)) -> do
                            liftIO $ print drawing
                            dWithStrokes <- liftIO $ Acid.query acid $ GetDrawing bid drawing
                            cdb <- liftIO $ connect dbConnectInfo
                            d <- liftIO $ case dWithStrokes of
                                Just ss -> Drawing.submit cdb ss bid
                                Nothing -> return Nothing
                            liftIO $ print d
                            liftIO $ Acid.update acid $ RemoveDrawing bid drawing
                        _ -> return ()
                    let s' = removeClient bid client s
                    broadcastBoard bid "disconnected" broadcast s'
                    return s'