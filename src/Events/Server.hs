{-# LANGUAGE OverloadedStrings #-}
module Events.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO
import qualified Network.Wai.Handler.Warp as Warp

import IAM.Client

import Events.Config
import Events.DB (connectToDatabase)
import Events.Server.App
import Events.Server.Migrations
import Events.Server.Session
import Events.Server.State


runServer :: Config -> IO ()
runServer conf = do
  iamConfig <- iamClientConfigEnv
  iamClient <- newIAMClient iamConfig
  db <- connectToDatabase $ configPostgres conf
  state <- atomically $ initState conf iamClient db

  _ <- forkIO $ runSessionManager state
  runDatabaseMigrations (configOptsMigrations $ configOpts conf) db
  runServerWithState (configOptsPort $ configOpts conf) state


runServerWithState :: Int -> State -> IO ()
runServerWithState httpPort state = do
  putStrLn "Starting server on http://localhost:8080"
  hFlush stdout
  Warp.run httpPort $ app state
