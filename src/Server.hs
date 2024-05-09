{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.Wai.Middleware.RequestLogger
import Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import qualified Servant.Client as SC

import IAM.Client.Auth
import IAM.Client.Util

import API
import Auth (authContext)
import Config (getHost)
import Handlers
import Socket (websocketHandler)
import State


server :: State -> Server API
server state auth
  = sessionsHandler state auth
  :<|> usersHandler state auth
  :<|> groupsHandler state auth


app :: State -> Wai.Application
app state req respond' =
  let handler = websocketHandler state
   in case WaiWS.websocketsApp WS.defaultConnectionOptions handler req of
    Just response -> respond' response
    Nothing -> serveWithContext
      (Proxy :: Proxy API)
      (authContext (unStateHost state) (unStateClientEnv state))
      (server state)
      req respond'


runServer :: IO ()
runServer = do
  url <- serverUrl
  auth <- clientAuthInfo
  hostname <- getHost
  mgr <- newManager tlsManagerSettings { managerModifyRequest = clientAuth auth }
  state <- atomically $ initState hostname $ SC.mkClientEnv mgr url

  done <- newEmptyTMVarIO
  _ <- forkIO $ runServerWithState state done
  _ <- atomically $ takeTMVar done

  atomically $ takeTMVar done


runServerWithState :: State -> TMVar () -> IO ()
runServerWithState state done = do
  putStrLn "Starting server on http://localhost:8080"
  Warp.run 8080 $ logStdout $ app state
  atomically $ putTMVar done ()
