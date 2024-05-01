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
  = EventMessage Message
  | EventJoinGroup UUID
  | EventLeaveGroup UUID


instance FromJSON Event where
  parseJSON (Object o) = do
    eventType :: Text <- o .: "type"
    case eventType of
      "joinGroup" -> EventJoinGroup <$> o .: "group"
      "leaveGroup" -> EventLeaveGroup <$> o .: "group"
      _ -> EventMessage <$> parseJSON (Object o)
  parseJSON _ = fail "Expected an object"


instance ToJSON Event where
  toJSON (EventMessage msg) = toJSON msg
  toJSON (EventJoinGroup group) =
    object ["type" .= ("joinGroup" :: Text), "group" .= group]
  toJSON (EventLeaveGroup group) =
    object ["type" .= ("leaveGroup" :: Text), "group" .= group]
