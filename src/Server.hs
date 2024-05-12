{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import System.IO
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Client as SC

import IAM.Client.Auth
import IAM.Client.Util

import Config (getHost)
import Server.App
import Server.Session
import State


runServer :: IO ()
runServer = do
  url <- serverUrl
  auth <- clientAuthInfo
  hostname <- getHost
  mgr <- newManager tlsManagerSettings { managerModifyRequest = clientAuth auth }
  state <- atomically $ initState hostname $ SC.mkClientEnv mgr url
  _ <- forkIO $ runSessionManager state
  runServerWithState state


runServerWithState :: State -> IO ()
runServerWithState state = do
  putStrLn "Starting server on http://localhost:8080"
  hFlush stdout
  Warp.run 8080 $ app state
