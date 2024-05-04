{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Event
  ( Event(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)

import Message


data Event
  = EventMessage !Message
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
      Just unrecognized -> fail $ "Unrecognized event type: " ++ show unrecognized
  parseJSON _ = fail "Expected an object"


instance ToJSON Event where
  toJSON (EventMessage msg) = toJSON msg
  toJSON (EventJoinGroup group) =
    object ["type" .= ("joinGroup" :: Text), "group" .= group]
  toJSON (EventLeaveGroup group) =
    object ["type" .= ("leaveGroup" :: Text), "group" .= group]
