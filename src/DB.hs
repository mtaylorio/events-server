{-# LANGUAGE OverloadedStrings #-}
module DB
  ( module DB
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E


data DBTopic = DBTopic
  { dbTopicId :: UUID
  , dbTopicBroadcast :: Bool
  , dbTopicCreated :: UTCTime
  } deriving (Show)


insertTopic :: Statement DBTopic ()
insertTopic = Statement sql encoder decoder True
  where
    sql = "INSERT INTO topics (uuid, broadcast, created_at) VALUES ($1, $2, $3)"
    encoder = topicEncoder
    decoder = D.noResult


selectTopics :: Statement () [DBTopic]
selectTopics = Statement sql encoder decoder True
  where
    sql = "SELECT uuid, broadcast, created_at FROM topics"
    encoder = E.noParams
    decoder = D.rowList topicDecoder


topicDecoder :: D.Row DBTopic
topicDecoder = DBTopic
  <$> D.column (D.nonNullable D.uuid)
  <*> D.column (D.nonNullable D.bool)
  <*> D.column (D.nonNullable D.timestamptz)


topicEncoder :: E.Params DBTopic
topicEncoder =
  (dbTopicId >$< E.param (E.nonNullable E.uuid)) <>
  (dbTopicBroadcast >$< E.param (E.nonNullable E.bool)) <>
  (dbTopicCreated >$< E.param (E.nonNullable E.timestamptz))
