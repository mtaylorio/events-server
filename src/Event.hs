{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Event
  ( Event(..)
  , EventData
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.Aeson.KeyMap as KM

import Message


type EventData = KM.KeyMap Value


data Event
  = EventPublish !UUID !EventData
  | EventSubscribe !UUID
  | EventUnsubscribe !UUID
  | EventMessage !Message
  | EventJoinGroup !UUID
  | EventLeaveGroup !UUID
  deriving (Eq, Show)


instance FromJSON Event where
  parseJSON (Object o) = do
    eventType :: Maybe Text <- o .:? "type"
    case eventType of
      Nothing -> EventMessage <$> parseJSON (Object o)
      Just "message" -> EventMessage <$> parseJSON (Object o)
      Just "joinGroup" -> EventJoinGroup <$> o .: "group"
      Just "leaveGroup" -> EventLeaveGroup <$> o .: "group"
      Just "publish" -> EventPublish <$> o .: "topic" <*> o .: "data"
      Just "subscribe" -> EventSubscribe <$> o .: "topic"
      Just "unsubscribe" -> EventUnsubscribe <$> o .: "topic"
      Just unrecognized -> fail $ "Unrecognized event type: " ++ show unrecognized
  parseJSON _ = fail "Expected an object"


instance ToJSON Event where
  toJSON (EventPublish topic data') =
    object ["type" .= ("publish" :: Text), "topic" .= topic, "data" .= data']
  toJSON (EventSubscribe topic) =
    object ["type" .= ("subscribe" :: Text), "topic" .= topic]
  toJSON (EventUnsubscribe topic) =
    object ["type" .= ("unsubscribe" :: Text), "topic" .= topic]
  toJSON (EventMessage msg) = toJSON msg
  toJSON (EventJoinGroup group) =
    object ["type" .= ("joinGroup" :: Text), "group" .= group]
  toJSON (EventLeaveGroup group) =
    object ["type" .= ("leaveGroup" :: Text), "group" .= group]
