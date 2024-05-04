{-# LANGUAGE OverloadedStrings #-}
module Recipient
  ( Recipient(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)


data Recipient
  = UserRecipient !Text
  | GroupRecipient !UUID
  | SessionRecipient !UUID
  deriving (Eq, Show)


instance FromJSON Recipient where
  parseJSON (Object o) = user <|> group <|> session where
    user = UserRecipient <$> o .: "user"
    group = GroupRecipient <$> o .: "group"
    session = SessionRecipient <$> o .: "session"
  parseJSON _ = fail "Expected an object"


instance ToJSON Recipient where
  toJSON (UserRecipient uuid) = object ["user" .= uuid]
  toJSON (GroupRecipient uuid) = object ["group" .= uuid]
  toJSON (SessionRecipient uuid) = object ["session" .= uuid]
