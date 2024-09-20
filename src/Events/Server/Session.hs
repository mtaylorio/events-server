module Events.Server.Session
  ( runSessionManager
  ) where

import Control.Concurrent (threadDelay)
import System.IO

import IAM.Client
import IAM.Session

import Events.Microseconds
import Events.Server.State


runSessionManager :: State -> IO ()
runSessionManager state = do
  let sessionsClient = mkCallerSessionsClient
  let createSession' = IAM.Client.createSession sessionsClient

  result <- iamRequest (unStateIAMClient state) createSession'
  case result of
    Left err -> do
      putStrLn $ "Error creating session: " ++ show err
      hFlush stdout
      threadDelay $ 15 * second
      runSessionManager state
    Right session -> do
      let token = createSessionToken session
      setSessionToken (unStateIAMClient state) $ Just token
      putStrLn "Created session"
      hFlush stdout
      sessionRefreshLoop state $ toSession session


sessionRefreshLoop :: State -> Session -> IO ()
sessionRefreshLoop state session = do
  let sessionsClient = mkCallerSessionsClient
  let sessionClient' = userSessionClient sessionsClient $ sessionId session
  let refreshSession' = IAM.Client.refreshSession sessionClient'

  result <- iamRequest (unStateIAMClient state) refreshSession'
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
