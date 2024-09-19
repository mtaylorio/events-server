module Server.Session
  ( runSessionManager
  ) where

import Control.Concurrent (threadDelay)
import Data.Text (unpack)
import Data.UUID (toText)
import System.Environment
import System.IO
import qualified Servant.Client as SC

import IAM.Client
import IAM.Session

import Microseconds
import State


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
  let sessionClient' = userSessionClient sessionsClient $ sessionId session
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
