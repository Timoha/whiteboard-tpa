{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Scotty as ST

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend, mconcat)
import Data.Maybe
import Data.Unique
import Data.Time
import qualified Data.HashMap.Strict as HashMap

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

import Network.Wai.Middleware.Static

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

type BoardId = Int

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
    toJSON (DrawingInfo drawingId firstName lastName) =
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

data DrawingInfo = DrawingInfo Int T.Text T.Text deriving Show


toDrawingInfo :: Drawing -> DrawingInfo
toDrawingInfo (Drawing did _ _ (User fn ln e) _ _) = DrawingInfo did fn ln

instance FromJSON DrawingInfo where
    parseJSON (Object v) = DrawingInfo <$>
                           v .: "drawingId" <*>
                           v .: "firstName" <*>
                           v .: "lastName"
    parseJSON _          = mzero


instance FromRow Drawing where
    fromRow = Drawing <$> field <*> field <*> field <*> (User <$> field <*> field <*> field) <*> field <*> field


insertDrawing :: Connection -> User -> BoardId -> IO [Drawing]
insertDrawing c (User firstName lastName email) bid =
  let q  = "insert into drawing (board_id, first_name, last_name, email, created) values (?, ?, ?, ?, NOW()) RETURNING *"
      vs = (bid, firstName, lastName, email)
  in query c q vs


newServerState :: ServerState
newServerState = HashMap.empty


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



main :: IO ()
main = do
    putStrLn "http://localhost:9160/"
    state <- newMVar newServerState
    api <- ST.scottyApp apiApp
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



apiApp :: ST.ScottyM ()
apiApp = do

    ST.middleware $ staticPolicy (noDots >-> addBase "public-dev")

    ST.get "/" $ ST.file "index.html"


    ST.post "/api/board/:bid/drawing" $ do
        bid <- ST.param "bid"
        b <- ST.jsonData
        liftIO $ putStrLn $ show b
        case b of
          Just usr@(User _ _ _) -> do
              cdb <- liftIO $ connect dbConnectInfo
              ds <- liftIO $ insertDrawing cdb usr bid
              if (not . null) ds
                  then ST.json $ (TL.decodeUtf8 . encode . toDrawingInfo) (head ds)
                  else ST.json $ (TL.decodeUtf8 . encode) (ServerError "cannot create drawing" HttpType.internalServerError500)
          _ -> ST.json $ (TL.decodeUtf8 . encode) (ServerError "invalid message format" HttpType.badRequest400)

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
