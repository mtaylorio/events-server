{-# LANGUAGE OverloadedStrings #-}
module API.Topics
  ( module API.Topics
  ) where

import Data.Aeson
import Data.UUID
import Data.Time.Clock


data TopicResponse = TopicResponse
  { topicResponseId :: UUID
  , topicResponseBroadcast :: Bool
  , topicResponseLogEvents :: Bool
  , topicResponseCreatedAt :: UTCTime
  } deriving (Eq, Show)


instance ToJSON TopicResponse where
  toJSON (TopicResponse id' broadcast logEvents createdAt) = object
    ["id" .= id'
    , "broadcast" .= broadcast
    , "logEvents" .= logEvents
    , "createdAt" .= createdAt
    ]


instance FromJSON TopicResponse where
  parseJSON = withObject "TopicResponse" $ \o -> TopicResponse
    <$> o .: "id"
    <*> o .: "broadcast"
    <*> o .: "logEvents"
    <*> o .: "createdAt"


newtype TopicsResponse
  = TopicsResponse { unTopicsResponseTopics :: [TopicResponse] }
  deriving (Eq, Show)


instance ToJSON TopicsResponse where
  toJSON (TopicsResponse topics) = object ["topics" .= topics]


instance FromJSON TopicsResponse where
  parseJSON = withObject "TopicsResponse" $ \o -> do
    topics <- o .: "topics"
    return $ TopicsResponse topics
