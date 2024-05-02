{-# LANGUAGE OverloadedStrings #-}
module Message
  ( Message(..)
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

import Recipient


data Message = Message
  { unMessageValue :: !(KM.KeyMap Value)
  , unMessageRecipient :: !Recipient
  } deriving (Eq, Show)


instance FromJSON Message where
  parseJSON (Object o) = do
    recipient <- o .: "recipient"
    return $ Message (KM.delete "recipient" o) recipient
  parseJSON _ = fail "Expected an object"


instance ToJSON Message where
  toJSON (Message value recipient) =
    Object $ KM.insert "recipient" (toJSON recipient) value
