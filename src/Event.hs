{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Event
  ( Event(..)
  , EventData(..)
  , EventWrapper(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL


data EventData = EventData
  { unEventId :: !UUID
  , unEventTopic :: !UUID
  , unEventCreated :: !UTCTime
  , unEventData :: !(KM.KeyMap Value)
  } deriving (Eq, Show)


instance FromJSON EventData where
  parseJSON = withObject "EventData" $ \o -> do
    id' <- o .: "id"
    topic <- o .: "topic"
    created <- o .: "created"
    return $ EventData id' topic created o


instance ToJSON EventData where
  toJSON (EventData id' topic created data') = Object
    $ KM.insert "id" (toJSON id')
    $ KM.insert "topic" (toJSON topic)
    $ KM.insert "created" (toJSON created)
    data'


data Event
  = EventPublish !EventData
  | EventSubscribe !UUID
  | EventUnsubscribe !UUID
  | EventReplay !UUID
  deriving (Eq, Show)


instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    type' :: Maybe Text <- o .:? "type"
    case type' of
      Nothing -> EventPublish <$> parseJSON (Object o)
      Just "publish" -> EventPublish <$> parseJSON (Object o)
      Just "subscribe" -> EventSubscribe <$> o .: "topic"
      Just "unsubscribe" -> EventUnsubscribe <$> o .: "topic"
      Just "replay" -> EventReplay <$> o .: "topic"
      Just _ -> fail "unknown event type"


instance ToJSON Event where
  toJSON (EventPublish data') =
    toJSON data'
  toJSON (EventSubscribe topic) =
    object ["type" .= ("subscribe" :: Text), "topic" .= topic]
  toJSON (EventUnsubscribe topic) =
    object ["type" .= ("unsubscribe" :: Text), "topic" .= topic]
  toJSON (EventReplay topic) =
    object ["type" .= ("replay" :: Text), "topic" .= topic]


data EventWrapper = EventWrapper
  { unEventBytes :: !BL.ByteString
  , unEvent :: !EventData
  } deriving (Eq, Show)
