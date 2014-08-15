import Web.Scotty.Trans (scottyAppT)

import Realtime (application, defaultServerState)
import Api (apiApp)
import DrawingProgress
import System.Environment

import Data.Acid (openLocalState)

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

import Control.Monad
import Control.Concurrent (newMVar)


main :: IO ()
main = do
    port  <- liftM read $ getEnv "PORT"
    state <- newMVar defaultServerState
    acid  <- openLocalState (BoardsState fixtures)
    api   <- scottyAppT id id $ apiApp acid state
    Warp.runSettings ((Warp.setTimeout 3600 . Warp.setPort port) Warp.defaultSettings)
       $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state acid) api





