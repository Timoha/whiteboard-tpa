import Web.Scotty (scottyApp)

import Realtime (application, defaultServerState)
import Api (apiApp)


import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

import Control.Concurrent (newMVar)


main :: IO ()
main = do
    putStrLn "http://localhost:9160/"
    state <- newMVar defaultServerState
    api <- scottyApp apiApp
    Warp.runSettings (Warp.setPort 9160 Warp.defaultSettings)
       $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) api





