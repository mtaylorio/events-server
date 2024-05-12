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


type EventData = KM.KeyMap Value


data Event
  = EventPublish !UUID !EventData
  | EventSubscribe !UUID
  | EventUnsubscribe !UUID
  deriving (Eq, Show)


instance FromJSON Event where
  parseJSON (Object o) = do
    eventType :: Maybe Text <- o .:? "type"
    case eventType of
      Just "publish" -> EventPublish <$> o .: "topic" <*> o .: "data"
      Just "subscribe" -> EventSubscribe <$> o .: "topic"
      Just "unsubscribe" -> EventUnsubscribe <$> o .: "topic"
      Just unrecognized -> fail $ "Unrecognized event type: " ++ show unrecognized
      Nothing -> fail "Missing event type"
  parseJSON _ = fail "Expected an object"


instance ToJSON Event where
  toJSON (EventPublish topic data') =
    object ["type" .= ("publish" :: Text), "topic" .= topic, "data" .= data']
  toJSON (EventSubscribe topic) =
    object ["type" .= ("subscribe" :: Text), "topic" .= topic]
  toJSON (EventUnsubscribe topic) =
    object ["type" .= ("unsubscribe" :: Text), "topic" .= topic]
