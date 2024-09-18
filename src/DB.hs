{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module DB
  ( module DB
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import Hasql.Transaction.Sessions
import qualified Data.Aeson.KeyMap as KM
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Pool as Pool

import Config (ConfigPostgres(..))
import Event


connectToDatabase :: ConfigPostgres -> IO Pool.Pool
connectToDatabase conf = do
  let settings = Connection.settings
        (encodeUtf8 $ configPostgresHost conf)
        (fromIntegral $ configPostgresPort conf)
        (encodeUtf8 $ configPostgresUser conf)
        (encodeUtf8 $ configPostgresPass conf)
        (encodeUtf8 $ configPostgresDb conf)
  Pool.acquire 3 1800 1800 settings


queryTopics :: Transaction [DBTopic]
queryTopics = statement () selectTopics


queryTopic :: UUID -> Transaction (Maybe DBTopic)
queryTopic topicId = statement topicId selectTopic


queryEventsCount :: UUID -> Transaction Int
queryEventsCount topicId = do
  fromIntegral <$> statement topicId selectEventsCount


queryEvents :: UUID -> Transaction [EventData]
queryEvents topicId = statement topicId selectEvents


queryEventsLimitOffset :: UUID -> Int -> Int -> Transaction [EventData]
queryEventsLimitOffset topicId limit offset = do
  statement (topicId, (fromIntegral limit, fromIntegral offset)) selectEventsLimitOffset


queryEvent :: UUID -> UUID -> Transaction (Maybe EventData)
queryEvent topicId eventId = statement (topicId, eventId) selectEvent


queryTopicLogEvents :: UUID -> Transaction (Maybe Bool)
queryTopicLogEvents topicId = statement topicId selectTopicLogEvents


addEvent :: UUID -> UUID -> KM.KeyMap Value -> UTCTime -> Transaction (Maybe EventData)
addEvent topicId eventId eventProps eventTime = do
  maybeTopic <- statement topicId selectTopic
  case maybeTopic of
    Nothing -> return Nothing
    Just topic -> do
      let eventData = EventData eventId prevId topicId eventTime eventProps
          prevId = dbTopicLastEventId topic
      statement eventData insertOnConflictUpdateEvent
      statement (topicId, Just eventId) updateTopicLastEventId
      return $ Just eventData


upsertEvent :: EventData -> Transaction ()
upsertEvent evt = do
  let topicId = unEventTopic evt
  maybeTopic <- statement topicId selectTopic
  case maybeTopic of
    Nothing -> return ()
    Just topic -> do
      let prevId = dbTopicLastEventId topic
      statement evt insertOnConflictUpdateEvent
      when (unEventPrev evt == prevId) $
        statement (topicId, Just $ unEventId evt) updateTopicLastEventId


upsertTopic :: DBTopic -> Transaction DBTopic
upsertTopic topic = do
  statement topic insertOnConflictUpdateTopic
  statement (dbTopicId topic) selectTopic >>= \case
    Nothing -> error "topic not found after upsert"
    Just topic' -> return topic'


deleteTopic :: UUID -> Transaction ()
deleteTopic topicId = do
  statement topicId deleteTopicEvents
  statement topicId deleteTopic'


deleteEvent :: UUID -> UUID -> Transaction ()
deleteEvent topicId eventId = do
  maybeEvent <- statement (topicId, eventId) selectEvent
  maybeTopic <- statement topicId selectTopic
  case (maybeEvent, maybeTopic) of
    (Just event, Just topic) -> do
      -- If the event is the last event in the topic, update the last event id
      when (Just (unEventId event) == dbTopicLastEventId topic) $
        statement (topicId, unEventPrev event) updateTopicLastEventId
      -- Update the next event's prev id
      statement (topicId, (eventId, unEventPrev event)) updateEventPrevId
      -- Delete the event
      statement (topicId, eventId) deleteEvent'
    (_, _) -> return ()


runQuery :: Pool.Pool -> Transaction a -> IO (Either Pool.UsageError a)
runQuery pool tx = Pool.use pool $ transaction Serializable Read tx


runUpdate :: Pool.Pool -> Transaction a -> IO (Either Pool.UsageError a)
runUpdate pool tx = Pool.use pool $ transaction Serializable Write tx


data DBTopic = DBTopic
  { dbTopicId :: !UUID
  , dbTopicBroadcast :: !Bool
  , dbTopicLogEvents :: !Bool
  , dbTopicCreated :: !UTCTime
  , dbTopicLastEventId :: !(Maybe UUID)
  } deriving (Show)


selectTopics :: Statement () [DBTopic]
selectTopics = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, broadcast, log_events, created_at, last_event_uuid FROM topics"
    encoder = E.noParams
    decoder = D.rowList topicDecoder


selectTopic :: Statement UUID (Maybe DBTopic)
selectTopic = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, broadcast, log_events, created_at, last_event_uuid FROM topics WHERE uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe topicDecoder


selectEventsCount :: Statement UUID Int32
selectEventsCount = Statement sql encoder decoder True
  where
    sql = "SELECT COUNT(*) FROM events WHERE topic_uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.int4))


selectEvents :: Statement UUID [EventData]
selectEvents = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, prev, topic_uuid, created_at, payload \
          \  FROM events WHERE topic_uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowList eventDataDecoder


selectEventsLimitOffset :: Statement (UUID, (Int32, Int32)) [EventData]
selectEventsLimitOffset = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, prev, topic_uuid, created_at, payload \
          \  FROM events WHERE topic_uuid = $1 \
          \  ORDER BY created_at DESC LIMIT $2 OFFSET $3"
    encoder = (fst >$< E.param (E.nonNullable E.uuid)) <>
              ((fst . snd) >$< E.param (E.nonNullable E.int4)) <>
              ((snd . snd) >$< E.param (E.nonNullable E.int4))
    decoder = D.rowList eventDataDecoder


selectEvent :: Statement (UUID, UUID) (Maybe EventData)
selectEvent = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, prev, topic_uuid, created_at, payload \
          \  FROM events WHERE topic_uuid = $1 AND uuid = $2"
    encoder = (fst >$< E.param (E.nonNullable E.uuid)) <>
              (snd >$< E.param (E.nonNullable E.uuid))
    decoder = D.rowMaybe eventDataDecoder


selectTopicLogEvents :: Statement UUID (Maybe Bool)
selectTopicLogEvents = Statement sql encoder decoder True
  where
    sql = "SELECT log_events FROM topics WHERE uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe (D.column (D.nonNullable D.bool))


updateEventPrevId :: Statement (UUID, (UUID, Maybe UUID)) ()
updateEventPrevId = Statement sql encoder decoder True
  where
    sql = "UPDATE events SET prev = $3 WHERE topic_uuid = $1 AND uuid = $2"
    encoder = (fst >$< E.param (E.nonNullable E.uuid)) <>
              ((fst . snd) >$< E.param (E.nonNullable E.uuid)) <>
              ((snd . snd) >$< E.param (E.nullable E.uuid))
    decoder = D.noResult


updateTopicLastEventId :: Statement (UUID, Maybe UUID) ()
updateTopicLastEventId = Statement sql encoder decoder True
  where
    sql = "UPDATE topics SET last_event_uuid = $2 WHERE uuid = $1"
    encoder = (fst >$< E.param (E.nonNullable E.uuid)) <>
              (snd >$< E.param (E.nullable E.uuid))
    decoder = D.noResult


updateTopicLogEvents' :: Statement (UUID, Bool) ()
updateTopicLogEvents' = Statement sql encoder decoder True
  where
    sql = "UPDATE topics SET log_events = $2 WHERE uuid = $1"
    encoder = uuidBoolEncoder
    decoder = D.noResult


insertOnConflictUpdateTopic :: Statement DBTopic ()
insertOnConflictUpdateTopic = Statement sql encoder decoder True
  where
    sql = "INSERT INTO topics \
          \  (uuid, broadcast, log_events, created_at, last_event_uuid) \
          \  VALUES ($1, $2, $3, $4) \
          \  ON CONFLICT (uuid) DO UPDATE SET \
          \  broadcast = EXCLUDED.broadcast, \
          \  log_events = EXCLUDED.log_events"
    encoder = topicEncoder
    decoder = D.noResult


insertOnConflictUpdateEvent :: Statement EventData ()
insertOnConflictUpdateEvent = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events \
          \  (uuid, topic_uuid, created_at, payload) \
          \  VALUES ($1, $2, $3, $4) \
          \  ON CONFLICT (uuid) DO UPDATE SET \
          \  payload = EXCLUDED.payload"
    encoder = eventDataEncoder
    decoder = D.noResult


deleteTopicEvents :: Statement UUID ()
deleteTopicEvents = Statement sql encoder decoder True
  where
    sql = "DELETE FROM events WHERE topic_uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.noResult


deleteEvent' :: Statement (UUID, UUID) ()
deleteEvent' = Statement sql encoder decoder True
  where
    sql = "DELETE FROM events WHERE topic_uuid = $1 AND uuid = $2"
    encoder = (fst >$< E.param (E.nonNullable E.uuid)) <>
              (snd >$< E.param (E.nonNullable E.uuid))
    decoder = D.noResult


deleteTopic' :: Statement UUID ()
deleteTopic' = Statement sql encoder decoder True
  where
    sql = "DELETE FROM topics WHERE uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.noResult


topicDecoder :: D.Row DBTopic
topicDecoder = DBTopic
  <$> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.timestamptz)
  <*> D.column (D.nullable D.uuid)


topicEncoder :: E.Params DBTopic
topicEncoder =
  (dbTopicId >$< E.param (E.nonNullable E.uuid)) <>
  (dbTopicBroadcast >$< E.param (E.nonNullable E.bool)) <>
  (dbTopicLogEvents >$< E.param (E.nonNullable E.bool)) <>
  (dbTopicCreated >$< E.param (E.nonNullable E.timestamptz)) <>
  (dbTopicLastEventId >$< E.param (E.nullable E.uuid))


uuidBoolEncoder :: E.Params (UUID, Bool)
uuidBoolEncoder =
  (fst >$< E.param (E.nonNullable E.uuid)) <>
  (snd >$< E.param (E.nonNullable E.bool))


eventDataDecoder :: D.Row EventData
eventDataDecoder = EventData
  <$> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nullable D.uuid)
  <*> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nonNullable D.timestamptz)
  <*> (jsonValueToObject <$> D.column (D.nonNullable D.jsonb))
  where
    jsonValueToObject (Object obj) = obj
    jsonValueToObject _ = KM.empty


eventDataEncoder :: E.Params EventData
eventDataEncoder =
  (unEventId >$< E.param (E.nonNullable E.uuid)) <>
  (unEventTopic >$< E.param (E.nonNullable E.uuid)) <>
  (unEventCreated >$< E.param (E.nonNullable E.timestamptz)) <>
  ((Object . unEventData) >$< E.param (E.nonNullable E.jsonb))
