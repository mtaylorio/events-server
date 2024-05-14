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

import Config
import DB (connectToDatabase)
import Server.App
import Server.Migrations
import Server.Session
import State


runServer :: Config -> IO ()
runServer conf = do
  url <- serverUrl
  auth <- clientAuthInfo
  db <- connectToDatabase $ configPostgres conf

  mgr <- newManager tlsManagerSettings { managerModifyRequest = clientAuth auth }
  state <- atomically $ initState conf (SC.mkClientEnv mgr url) db

  _ <- forkIO $ runSessionManager state
  runDatabaseMigrations (configOptsMigrations $ configOpts conf) db
  runServerWithState (configOptsPort $ configOpts conf) state


runServerWithState :: Int -> State -> IO ()
runServerWithState httpPort state = do
  putStrLn "Starting server on http://localhost:8080"
  hFlush stdout
  Warp.run httpPort $ app state
