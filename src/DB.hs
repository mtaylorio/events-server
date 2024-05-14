{-# LANGUAGE OverloadedStrings #-}
module DB
  ( module DB
  ) where

import Data.Aeson (Value(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
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


queryEvents :: UUID -> Transaction [EventData]
queryEvents topicId = statement topicId selectEvents


queryTopicLogEvents :: UUID -> Transaction (Maybe Bool)
queryTopicLogEvents topicId = statement topicId selectTopicLogEvents


updateTopicLogEvents :: UUID -> Bool -> Transaction ()
updateTopicLogEvents topicId logEvents =
  statement (topicId, logEvents) updateTopicLogEvents'


upsertEvent :: EventData -> Transaction ()
upsertEvent evt = statement evt insertOnConflictUpdateEvent


upsertTopicBroadcast :: DBTopic -> Transaction ()
upsertTopicBroadcast topic = statement topic insertOnConflictUpdateTopicBroadcast


runQuery :: Pool.Pool -> Transaction a -> IO (Either Pool.UsageError a)
runQuery pool tx = Pool.use pool $ transaction Serializable Read tx


runUpdate :: Pool.Pool -> Transaction a -> IO (Either Pool.UsageError a)
runUpdate pool tx = Pool.use pool $ transaction Serializable Write tx


data DBTopic = DBTopic
  { dbTopicId :: UUID
  , dbTopicBroadcast :: Bool
  , dbTopicLogEvents :: Bool
  , dbTopicCreated :: UTCTime
  } deriving (Show)


selectTopics :: Statement () [DBTopic]
selectTopics = Statement sql encoder decoder True
  where
    sql = "SELECT (uuid, broadcast, log_events, created_at) FROM topics"
    encoder = E.noParams
    decoder = D.rowList topicDecoder


selectEvents :: Statement UUID [EventData]
selectEvents = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, topic_uuid, created_at, payload \
          \  FROM events WHERE topic_uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowList eventDataDecoder


selectTopicLogEvents :: Statement UUID (Maybe Bool)
selectTopicLogEvents = Statement sql encoder decoder True
  where
    sql = "SELECT log_events FROM topics WHERE uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe (D.column (D.nonNullable D.bool))


updateTopicLogEvents' :: Statement (UUID, Bool) ()
updateTopicLogEvents' = Statement sql encoder decoder True
  where
    sql = "UPDATE topics SET log_events = $2 WHERE uuid = $1"
    encoder = uuidBoolEncoder
    decoder = D.noResult


insertOnConflictUpdateTopicBroadcast :: Statement DBTopic ()
insertOnConflictUpdateTopicBroadcast = Statement sql encoder decoder True
  where
    sql = "INSERT INTO topics \
          \  (uuid, broadcast, log_events, created_at) \
          \  VALUES ($1, $2, $3, $4) \
          \  ON CONFLICT (uuid) DO UPDATE SET \
          \  broadcast = EXCLUDED.broadcast"
    encoder = topicEncoder
    decoder = D.noResult


insertOnConflictUpdateEvent :: Statement EventData ()
insertOnConflictUpdateEvent = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events \
          \  (uuid, topic_uuid, created_at, payload) \
          \  VALUES ($1, $2, $3, $4) \
          \  ON CONFLICT (uuid) DO NOTHING"
    encoder = eventDataEncoder
    decoder = D.noResult


topicDecoder :: D.Row DBTopic
topicDecoder = DBTopic
  <$> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.timestamptz)


topicEncoder :: E.Params DBTopic
topicEncoder =
  (dbTopicId >$< E.param (E.nonNullable E.uuid)) <>
  (dbTopicBroadcast >$< E.param (E.nonNullable E.bool)) <>
  (dbTopicLogEvents >$< E.param (E.nonNullable E.bool)) <>
  (dbTopicCreated >$< E.param (E.nonNullable E.timestamptz))


uuidBoolEncoder :: E.Params (UUID, Bool)
uuidBoolEncoder =
  (fst >$< E.param (E.nonNullable E.uuid)) <>
  (snd >$< E.param (E.nonNullable E.bool))


eventDataDecoder :: D.Row EventData
eventDataDecoder = EventData
  <$> D.column (D.nonNullable D.uuid)
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
