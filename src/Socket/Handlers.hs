module Socket.Handlers
  ( module Socket.Handlers
  ) where

import Control.Concurrent.STM
import Data.Aeson
import Data.UUID
import System.IO
import qualified Network.WebSockets as WS

import IAM.Policy (Action(..))

import Client
import DB
import Event
import Socket.Auth
import State
import Topic


authFailedMsg :: String -> Client -> UUID -> String
authFailedMsg action client topic
  = "Authorization failed for " ++ action
  ++ " on topic " ++ show topic
  ++ " by user " ++ show (unClientUser client)


handlePublish :: State -> TVar Client -> EventWrapper -> IO ()
handlePublish state client' evt = do
  client <- readTVarIO client'
  authorized <- authorizeTopic state client Write topic
  if not authorized
    then do
      hPutStrLn stderr $ authFailedMsg "publish" client topic
      hFlush stderr
    else do
      publish (unStateTopics state) topic evt
      result0 <- runQuery db $ queryTopicLogEvents topic
      case result0 of
        Right (Just False) -> return ()
        Right (Just True) -> do
          result1 <- runUpdate db $ upsertEvent $ unEvent evt
          case result1 of
            Left err -> do
              putStrLn $ "Error inserting event: " ++ show err
              return ()
            Right _ -> return ()
        Right Nothing -> do
          putStrLn $ "Topic not found: " ++ show topic
          return ()
        Left err -> do
          putStrLn $ "Error selecting topic: " ++ show err
          return ()
  where
  topic = unEventTopic $ unEvent evt
  db = unStateDatabase state


handleSubscribe :: State -> UUID -> TVar Client -> IO ()
handleSubscribe state topic client = do
  client' <- readTVarIO client
  authorized <- authorizeTopic state client' Read topic
  if not authorized
    then do
      hPutStrLn stderr $ authFailedMsg "subscribe" client' topic
      hFlush stderr
    else do
      unsubscribe <- subscribe (unStateTopics state) topic handleEvent
      case unsubscribe of
        Just unsub -> atomically $ modifyTVar' client $ addSubscription topic unsub
        Nothing -> return ()
  where
  handleEvent :: EventWrapper -> IO ()
  handleEvent evt = do
    client' <- readTVarIO client
    WS.sendTextData (unClientConn client') (unEventBytes evt)


handleUnsubscribe :: UUID -> TVar Client -> IO ()
handleUnsubscribe topic client = do
  client' <- readTVarIO client
  case filter ((== topic) . snd) $ unClientSubscriptions client' of
    [] -> return ()
    ((unsub, _):_) -> do
      unsub
      atomically $ modifyTVar' client $ removeSubscription topic


handleReplay :: State -> UUID -> TVar Client -> IO ()
handleReplay state topic client = do
  result <- runQuery db $ queryEvents topic
  case result of
    Right events -> do
      client' <- readTVarIO client
      mapM_ (WS.sendTextData (unClientConn client') . encode) events
    Left err -> do
      putStrLn $ "Error selecting events: " ++ show err
      return ()
  where
  db = unStateDatabase state
