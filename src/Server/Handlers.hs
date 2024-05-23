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


topicsHandler :: State -> Auth -> Handler TopicsResponse
topicsHandler state (Authenticated{}) = do
  topics <- liftIO $ runQuery db queryTopics
  case topics of
    Left err -> do
      liftIO $ putStrLn $ "Error querying topics: " ++ show err
      throwError err500
    Right topics' -> do
      return $ TopicsResponse $ map topicResponse topics'
  where
  db = unStateDatabase state
  topicResponse (DBTopic topic broadcast logEvents created) =
    TopicResponse topic broadcast logEvents created
topicsHandler _ _ = throwError err401


topicHandler :: State -> Auth -> UUID -> Handler TopicResponse
topicHandler state (Authenticated{}) topicId = do
  topic' <- liftIO $ runQuery db $ queryTopic topicId
  case topic' of
    Left err -> do
      liftIO $ putStrLn $ "Error querying topic: " ++ show err
      throwError err500
    Right Nothing -> throwError err404
    Right (Just (DBTopic topicId' broadcast logEvents created)) ->
      return $ TopicResponse topicId' broadcast logEvents created
  where
  db = unStateDatabase state
topicHandler _ _ _ = throwError err401


createTopicHandler :: State -> Auth -> CreateTopic -> Handler TopicResponse
createTopicHandler state (Authenticated{}) createTopic = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runUpdate db $ upsertTopic $ dbTopic now
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting topic: " ++ show err
      throwError err500
    Right _ -> do
      return $ TopicResponse
        (createTopicId createTopic)
        (createTopicBroadcast createTopic)
        (createTopicLogEvents createTopic)
        now
  where
  db = unStateDatabase state
  dbTopic = DBTopic
    (createTopicId createTopic)
    (createTopicBroadcast createTopic)
    (createTopicLogEvents createTopic)
createTopicHandler _ _ _ = throwError err401


deleteTopicHandler :: State -> Auth -> UUID -> Handler NoContent
deleteTopicHandler state (Authenticated{}) topic = do
  result <- liftIO $ runUpdate db $ deleteTopic topic
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error deleting topic: " ++ show err
      throwError err500
    Right _ -> do
      liftIO $ atomically $ deleteTopicFromState state topic
      return NoContent
  where
  db = unStateDatabase state
deleteTopicHandler _ _ _ = throwError err401


updateTopicHandler :: State -> Auth -> UUID -> UpdateTopic -> Handler TopicResponse
updateTopicHandler state (Authenticated{}) topic updateTopic = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runUpdate db $ upsertTopic $ dbTopic now
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting topic: " ++ show err
      throwError err500
    Right _ -> do
      return $ TopicResponse
        topic
        (updateTopicBroadcast updateTopic)
        (updateTopicLogEvents updateTopic)
        now
  where
  db = unStateDatabase state
  dbTopic =
    DBTopic topic (updateTopicBroadcast updateTopic) (updateTopicLogEvents updateTopic)
updateTopicHandler _ _ _ _ = throwError err401
