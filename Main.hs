{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend, mconcat)
import Data.Maybe
import Data.Unique
import Data.Time
import Data.HashMap.Strict as HashMap

import Control.Exception (fromException, handle)
import Control.Monad
import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow


import Network.Wai.Middleware.RequestLogger
import qualified Network.HTTP.Types as HttpType
import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)


dbConnectInfo = ConnectInfo "ec2-54-197-238-8.compute-1.amazonaws.com" 5432 "wqjjxnyklchrwn" "4s4YvSJclwr1pUQMo36p4rv4At" "db86ci3toitofh"

data ClientMessage = ClientMessage { drawing :: Maybe DrawingInfo
                                   , element :: Value
                                   , action :: T.Text
                                   } deriving Show

instance ToJSON ClientMessage where
    toJSON (ClientMessage drawing element action) =
        object [ "drawing" .= drawing
               , "element" .= element
               , "action" .= action ]




--data Element = Stroke | [Points] | [Stroke]


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

type Client = Int

type ServerState = HashMap.HashMap Client WS.Connection


instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage <$>
                           v .: "drawing" <*>
                           v .: "element" <*>
                           v .: "action"
    parseJSON _          = mzero


instance FromJSON User where
    parseJSON (Object v) = User <$>
                           v .: "firstName" <*>
                           v .: "lastName" <*>
                           v .: "email"
    parseJSON _          = mzero


instance ToJSON DrawingInfo where
    toJSON (DrawingInfo drawingId firstName lastName _) =
      object [ "drawingId" .= drawingId
             , "firstName" .= firstName
             , "lastName" .= lastName ]


data User = User { firstName :: T.Text
                 , lastName :: T.Text
                 , email :: T.Text
                 } deriving Show

data Drawing = Drawing { drawingId :: Int
                       , boardId :: Int
                       , strokes :: Maybe T.Text
                       , user :: User
                       , created :: UTCTime
                       , submitted :: Maybe UTCTime
                       } deriving Show

data DrawingInfo = DrawingInfo (Maybe Int) T.Text T.Text T.Text deriving Show


toDrawingInfo :: Drawing -> DrawingInfo
toDrawingInfo (Drawing did _ _ (User fn ln e) _ _) = DrawingInfo (Just did) fn ln e

instance FromJSON DrawingInfo where
    parseJSON (Object v) = DrawingInfo <$>
                           v .:? "drawingId" <*>
                           v .: "firstName" <*>
                           v .: "lastName" <*>
                           v .: "email"
    parseJSON _          = mzero


instance FromRow Drawing where
    fromRow = Drawing <$> field <*> field <*> field <*> (User <$> field <*> field <*> field) <*> field <*> field


insertDrawing :: Connection -> DrawingInfo -> IO [Drawing]
insertDrawing c (DrawingInfo _ firstName lastName email) =
  let q  = "insert into drawing (board_id, first_name, last_name, email, created) values (?, ?, ?, ?, NOW()) RETURNING *"
      vs = (1 :: Int, firstName, lastName, email)
  in query c q vs


newServerState :: ServerState
newServerState = HashMap.empty


numClients :: ServerState -> Int
numClients = size

addClient :: Int -> WS.Connection -> ServerState -> ServerState
addClient = HashMap.insert


removeClient :: Int -> ServerState -> ServerState
removeClient = HashMap.delete


broadcast :: TL.Text -> ServerState -> IO ()
broadcast message clients = do
    TL.putStrLn message
    forM_ (elems clients) $ \c -> forkIO $ WS.sendTextData c message

broadcastOther :: TL.Text -> Client -> ServerState -> IO ()
broadcastOther message me clients = broadcast message $ HashMap.filterWithKey (\k _ -> k /= me) clients



main :: IO ()
main = do
    putStrLn "http://localhost:9160/"
    state <- newMVar newServerState
    api <- scottyApp apiApp
    Warp.runSettings (Warp.setPort 9160 Warp.defaultSettings)
       $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) api




data Message = NewClient
             | NewDrawing DrawingInfo
--             | RemoveStroke Stroke User
--             | AddPoints [Points] User
--             | AddStrokes [Stroke] User
             | NoOp




handleMessage :: ClientMessage -> Maybe Message
handleMessage msg =
    case action msg of
        "NewClient" -> Just NewClient
        "NewDrawing" -> fmap NewDrawing d
        "NoOpServer" -> Just NoOp
        _ -> Nothing
    where
        d = drawing msg



apiApp :: ScottyM ()
apiApp = do
    get "/" $ text "YOLO SWAG"

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
            Just m -> case handleMessage m of
                Just (NewDrawing d) -> do
                  cdb <- connect dbConnectInfo
                  ds <- insertDrawing cdb d
                  (WS.sendTextData conn . encode) (ClientMessage ((Just . toDrawingInfo) (head ds)) Null "CanDraw")
                Just NoOp -> return ()
                _ -> liftIO $ readMVar state >>= broadcastOther (TL.decodeUtf8 msg) client
            Nothing -> do (WS.sendTextData conn . encode) (ServerError "invalid message format" HttpType.badRequest400)
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
