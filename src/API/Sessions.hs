{-# LANGUAGE OverloadedStrings #-}
module API.Sessions
  ( module API.Sessions
  ) where

import Data.Aeson
import Data.UUID


data SessionResponse = SessionResponse
  { sessionResponseUser :: UUID
  , sessionResponseSession :: UUID
  , sessionResponseTopics :: [UUID]
  } deriving (Eq, Show)


instance ToJSON SessionResponse where
  toJSON (SessionResponse user session topics) =
    object ["user" .= user, "session" .= session, "topics" .= topics]


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o ->
    SessionResponse <$> o .: "user" <*> o .: "session" <*> o .: "topics"


newtype SessionsResponse
  = SessionsResponse { unSessionsResponseSessions :: [UUID] }
  deriving (Eq, Show)


instance ToJSON SessionsResponse where
  toJSON (SessionsResponse sessions) = object ["sessions" .= sessions]


instance FromJSON SessionsResponse where
  parseJSON = withObject "SessionsResponse" $ \o -> do
    sessions <- o .: "sessions"
    return $ SessionsResponse sessions
