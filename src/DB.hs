{-# LANGUAGE OverloadedStrings #-}
module DB
  ( module DB
  ) where

import Data.Aeson (Value(..))
import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import Event


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


selectTopicLogEvents :: Statement UUID (Maybe Bool)
selectTopicLogEvents = Statement sql encoder decoder True
  where
    sql = "SELECT log_events FROM topics WHERE uuid = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe (D.column (D.nonNullable D.bool))


updateTopicLogEvents :: Statement (UUID, Bool) ()
updateTopicLogEvents = Statement sql encoder decoder True
  where
    sql = "UPDATE topics SET log_events = $2 WHERE uuid = $1"
    encoder = uuidBoolEncoder
    decoder = D.noResult


upsertTopicBroadcast :: Statement DBTopic ()
upsertTopicBroadcast = Statement sql encoder decoder True
  where
    sql = "INSERT INTO topics \
          \  (uuid, broadcast, log_events, created_at) \
          \  VALUES ($1, $2, $3, $4) \
          \  ON CONFLICT (uuid) DO UPDATE SET \
          \  broadcast = EXCLUDED.broadcast"
    encoder = topicEncoder
    decoder = D.noResult


upsertEvent :: Statement EventData ()
upsertEvent = Statement sql encoder decoder True
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
  <*> jsonKeyMapValueDecoder


jsonKeyMapValueDecoder :: D.Row (KM.KeyMap Value)
jsonKeyMapValueDecoder = do
  json <- D.column (D.nonNullable D.jsonb)
  case json of
    (Object o) -> return o
    _ -> fail "expected JSON object"


eventDataEncoder :: E.Params EventData
eventDataEncoder =
  (unEventId >$< E.param (E.nonNullable E.uuid)) <>
  (unEventTopic >$< E.param (E.nonNullable E.uuid)) <>
  (unEventCreated >$< E.param (E.nonNullable E.timestamptz)) <>
  ((Object . unEventData) >$< E.param (E.nonNullable E.jsonb))
