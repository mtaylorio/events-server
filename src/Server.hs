{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Text (unpack)
import Data.UUID (toText)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.Wai.Middleware.RequestLogger
import Servant
import System.Environment
import System.IO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS
import qualified Servant.Client as SC

import IAM.Client
import IAM.Client.Auth
import IAM.Client.Util

import IAM.Session

import API
import Auth
import Config (getHost)
import Handlers
import Microseconds
import Socket (websocketHandler)
import State


server :: State -> Server API
server state auth
  = usersHandler state auth
  :<|> groupsHandler state auth
  :<|> sessionsHandler state auth
  :<|> sessionHandler state auth
  :<|> topicsServer state auth


topicsServer :: State -> Auth -> Server TopicsAPI
topicsServer state auth topic
  = createBroadcastTopicHandler state auth topic
  :<|> createSendReceiveTopicHandler state auth topic


app :: State -> Wai.Application
app state req respond' =
  let handler = websocketHandler state
   in case WaiWS.websocketsApp WS.defaultConnectionOptions handler req of
    Just response -> respond' response
    Nothing -> logStdout (
      serveWithContext
      (Proxy :: Proxy API)
      (authContext (unStateHost state) (unStateClientEnv state))
      (server state)
      ) req respond'


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


runSessionManager :: State -> IO ()
runSessionManager state = do
  let sessionsClient = mkCallerSessionsClient
  let createSession' = IAM.Client.createSession sessionsClient

  result <- SC.runClientM createSession' (unStateClientEnv state)
  case result of
    Left err -> do
      putStrLn $ "Error creating session: " ++ show err
      hFlush stdout
      threadDelay $ 15 * second
      runSessionManager state
    Right session -> do
      let sid = createSessionId session
      let token = createSessionToken session
      setEnv "MTAYLOR_IO_SESSION_ID" $ unpack $ toText $ unSessionId sid
      setEnv "MTAYLOR_IO_SESSION_TOKEN" $ unpack token
      putStrLn "Created session"
      hFlush stdout
      sessionRefreshLoop state $ toSession session


sessionRefreshLoop :: State -> Session -> IO ()
sessionRefreshLoop state session = do
  let sessionsClient = mkCallerSessionsClient
  let sessionClient' = sessionClient sessionsClient $ sessionId session
  let refreshSession' = IAM.Client.refreshSession sessionClient'

  result <- SC.runClientM refreshSession' (unStateClientEnv state)
  case result of
    Left err -> do
      putStrLn $ "Error refreshing session: " ++ show err
      hFlush stdout
      threadDelay minute
      sessionRefreshLoop state session
    Right session' -> do
      putStrLn "Refreshed session"
      hFlush stdout
      threadDelay minute
      sessionRefreshLoop state session'
