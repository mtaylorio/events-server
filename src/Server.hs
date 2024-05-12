{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import System.IO
import qualified Hasql.Connection as Connection
import qualified Hasql.Pool as Pool
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant.Client as SC

import IAM.Client.Auth
import IAM.Client.Util

import Config
import Server.App
import Server.Migrations
import Server.Session
import State


connectToDatabase :: ConfigPostgres -> IO Pool.Pool
connectToDatabase conf = do
  let settings = Connection.settings
        (encodeUtf8 $ configPostgresHost conf)
        (fromIntegral $ configPostgresPort conf)
        (encodeUtf8 $ configPostgresUser conf)
        (encodeUtf8 $ configPostgresPass conf)
        (encodeUtf8 $ configPostgresDb conf)
  Pool.acquire 3 1800 1800 settings


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
