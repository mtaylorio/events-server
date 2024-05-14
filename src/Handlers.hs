{-# LANGUAGE OverloadedStrings #-}
module Handlers
  ( module Handlers
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Time.Clock
import Data.UUID
import Servant
import qualified Data.Map as Map
import qualified Network.WebSockets as WS

import API
import Auth
import Client
import DB
import Event
import State
import Topic


handlePublish :: State -> EventWrapper -> IO ()
handlePublish state evt = do
  publish (unStateTopics state) topic evt
  result0 <- runQuery db $ queryTopicLogEvents topic
  case result0 of
    Right (Just False) -> return ()
    Right (Just True) -> do
      result1 <- runQuery db $ upsertEvent $ unEvent evt
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


sessionsHandler :: State -> Auth -> Handler SessionsResponse
sessionsHandler state (Authenticated{}) = do
  clients <- liftIO $ readTVarIO $ unStateClients state
  return $ SessionsResponse $ Map.keys clients
sessionsHandler _ _ = throwError err401


sessionHandler :: State -> Auth -> UUID -> Handler SessionResponse
sessionHandler state (Authenticated{}) session = do
  clients <- liftIO $ readTVarIO $ unStateClients state
  case Map.lookup session clients of
    Nothing -> throwError err404
    Just client -> do
      client' <- liftIO $ readTVarIO client
      let user = unClientUser client'
      return $ SessionResponse user session $ listSubscriptions client'
sessionHandler _ _ _ = throwError err401


createBroadcastTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createBroadcastTopicHandler state (Authenticated{}) = createTopicHandler state True
createBroadcastTopicHandler _ _ = \_ -> throwError err401


createSendReceiveTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createSendReceiveTopicHandler state (Authenticated{}) = createTopicHandler state False
createSendReceiveTopicHandler _ _ = \_ -> throwError err401


createTopicHandler :: State -> Bool -> UUID -> Handler NoContent
createTopicHandler state broadcast topic = do
  now <- liftIO getCurrentTime
  let dbTopic = DBTopic topic broadcast False now
  result <- liftIO $ runQuery db $ upsertTopicBroadcast dbTopic
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting topic: " ++ show err
      throwError err500
    Right _ -> do
      if broadcast
        then liftIO $ atomically $ createBroadcastTopic (unStateTopics state) topic
        else liftIO $ atomically $ createSendReceiveTopic (unStateTopics state) topic
      return NoContent
  where
  db = unStateDatabase state


logEventsHandler :: State -> Auth -> UUID -> Handler NoContent
logEventsHandler state (Authenticated{}) = setLogEventsHandler state True
logEventsHandler _ _ = \_ -> throwError err401


deleteLogEventsHandler :: State -> Auth -> UUID -> Handler NoContent
deleteLogEventsHandler state (Authenticated{}) = setLogEventsHandler state False
deleteLogEventsHandler _ _ = \_ -> throwError err401


setLogEventsHandler :: State -> Bool -> UUID -> Handler NoContent
setLogEventsHandler state logEvents topic = do
  result <- liftIO $ runQuery db $ updateTopicLogEvents topic logEvents
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting event: " ++ show err
      throwError err500
    Right _ ->
      return NoContent
  where
  db = unStateDatabase state
