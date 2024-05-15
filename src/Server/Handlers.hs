{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers
  ( module Server.Handlers
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Data.UUID
import Servant
import qualified Data.Map as Map

import API
import Client
import DB
import Server.Auth
import State
import Topic


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
  result <- liftIO $ runUpdate db $ upsertTopicBroadcast dbTopic
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
  result <- liftIO $ runUpdate db $ updateTopicLogEvents topic logEvents
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting event: " ++ show err
      throwError err500
    Right _ ->
      return NoContent
  where
  db = unStateDatabase state
