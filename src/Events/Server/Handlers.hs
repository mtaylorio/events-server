{-# LANGUAGE OverloadedStrings #-}
module Events.Server.Handlers
  ( module Events.Server.Handlers
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Maybe
import Data.Time.Clock
import Data.UUID
import Servant
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map

import Events.API
import Events.API.Helpers
import Events.DB
import Events.Event
import Events.Server.Auth
import Events.Server.Client
import Events.Server.State
import Events.Server.Version (version)
import Events.Status (Health(Healthy), StatusResponse(StatusResponse))


healthHandler :: State -> Handler StatusResponse
healthHandler _ = return $ StatusResponse Healthy version


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
  topicResponse (DBTopic topic broadcast logEvents created lastEventId) =
    TopicResponse topic broadcast logEvents created lastEventId
topicsHandler _ _ = throwError err401


topicHandler :: State -> Auth -> UUID -> Handler TopicResponse
topicHandler state (Authenticated{}) topicId = do
  topic' <- liftIO $ runQuery db $ queryTopic topicId
  case topic' of
    Left err -> do
      liftIO $ putStrLn $ "Error querying topic: " ++ show err
      throwError err500
    Right Nothing -> throwError err404
    Right (Just (DBTopic topicId' broadcast logEvents created lastEventId)) ->
      return $ TopicResponse topicId' broadcast logEvents created lastEventId
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
        Nothing
  where
  db = unStateDatabase state
  dbTopic timestamp = DBTopic
    (createTopicId createTopic)
    (createTopicBroadcast createTopic)
    (createTopicLogEvents createTopic)
    timestamp
    Nothing
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
    Right t -> do
      return $ TopicResponse
        (dbTopicId t)
        (dbTopicBroadcast t)
        (dbTopicLogEvents t)
        (dbTopicCreated t)
        (dbTopicLastEventId t)
  where
  db = unStateDatabase state
  dbTopic timestamp =
    DBTopic
      topic
      (updateTopicBroadcast updateTopic)
      (updateTopicLogEvents updateTopic)
      timestamp
      Nothing
updateTopicHandler _ _ _ _ = throwError err401


listEventsHandler ::
  State -> Auth -> UUID -> Maybe Int -> Maybe Int -> Handler (ListResponse EventData)
listEventsHandler state (Authenticated{}) topic maybeLimit maybeOffset = do
  let limit = fromMaybe 10 maybeLimit
  let offset = fromMaybe 0 maybeOffset
  events <- liftIO $ runQuery db $ queryEventsLimitOffset topic limit offset
  total <- liftIO $ runQuery db $ queryEventsCount topic
  case (events, total) of
    (Left err, _) -> do
      liftIO $ putStrLn $ "Error querying events: " ++ show err
      throwError err500
    (_, Left err) -> do
      liftIO $ putStrLn $ "Error querying events count: " ++ show err
      throwError err500
    (Right events', Right total') -> do
      return $ ListResponse events' total'
  where
  db = unStateDatabase state
listEventsHandler _ _ _ _ _ = throwError err401


getEventHandler :: State -> Auth -> UUID -> UUID -> Handler EventData
getEventHandler state (Authenticated{}) topic event = do
  maybeEvent <- liftIO $ runQuery db $ queryEvent topic event
  case maybeEvent of
    Left err -> do
      liftIO $ putStrLn $ "Error querying event: " ++ show err
      throwError err500
    Right Nothing ->
      throwError err404
    Right (Just event') -> do
      return event'
  where
  db = unStateDatabase state
getEventHandler _ _ _ _ = throwError err401


deleteEventHandler :: State -> Auth -> UUID -> UUID -> Handler NoContent
deleteEventHandler state (Authenticated{}) topic event = do
  result <- liftIO $ runUpdate db $ deleteEvent topic event
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error deleting event: " ++ show err
      throwError err500
    Right _ -> do
      return NoContent
  where
  db = unStateDatabase state
deleteEventHandler _ _ _ _ = throwError err401


upsertEventHandler ::
  State -> Auth -> UUID -> UUID -> KM.KeyMap Value -> Handler EventData
upsertEventHandler state (Authenticated{}) topic event data' = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runUpdate db $ addEvent topic event data' now
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error upserting event: " ++ show err
      throwError err500
    Right Nothing ->
      throwError err404
    Right (Just eventData) -> do
      return eventData
  where
  db = unStateDatabase state
upsertEventHandler _ _ _ _ _ = throwError err401
