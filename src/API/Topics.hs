{-# LANGUAGE OverloadedStrings #-}
module API.Topics
  ( module API.Topics
  ) where

import Data.Aeson
import Data.UUID
import Data.Time.Clock


data CreateTopic = CreateTopic
  { createTopicId :: !UUID
  , createTopicBroadcast :: !Bool
  , createTopicLogEvents :: !Bool
  } deriving (Eq, Show)


instance ToJSON CreateTopic where
  toJSON (CreateTopic id' broadcast logEvents) = object
    ["id" .= id'
    , "broadcast" .= broadcast
    , "logEvents" .= logEvents
    ]


instance FromJSON CreateTopic where
  parseJSON = withObject "CreateTopic" $ \o -> CreateTopic
    <$> o .: "id"
    <*> o .: "broadcast"
    <*> o .: "logEvents"


data UpdateTopic = UpdateTopic
  { updateTopicBroadcast :: !Bool
  , updateTopicLogEvents :: !Bool
  } deriving (Eq, Show)


instance ToJSON UpdateTopic where
  toJSON (UpdateTopic broadcast logEvents) = object
    ["broadcast" .= broadcast
    , "logEvents" .= logEvents
    ]


instance FromJSON UpdateTopic where
  parseJSON = withObject "UpdateTopic" $ \o -> UpdateTopic
    <$> o .: "broadcast"
    <*> o .: "logEvents"


data TopicResponse = TopicResponse
  { topicResponseId :: !UUID
  , topicResponseBroadcast :: !Bool
  , topicResponseLogEvents :: !Bool
  , topicResponseCreatedAt :: !UTCTime
  , topicResponseLastEventId :: !(Maybe UUID)
  } deriving (Eq, Show)


instance ToJSON TopicResponse where
  toJSON (TopicResponse id' broadcast logEvents createdAt lastEventId) = object
    ["id" .= id'
    , "broadcast" .= broadcast
    , "logEvents" .= logEvents
    , "createdAt" .= createdAt
    , "lastEventId" .= lastEventId
    ]


instance FromJSON TopicResponse where
  parseJSON = withObject "TopicResponse" $ \o -> TopicResponse
    <$> o .: "id"
    <*> o .: "broadcast"
    <*> o .: "logEvents"
    <*> o .: "createdAt"
    <*> o .: "lastEventId"


newtype TopicsResponse
  = TopicsResponse { unTopicsResponseTopics :: [TopicResponse] }
  deriving (Eq, Show)


instance ToJSON TopicsResponse where
  toJSON (TopicsResponse topics) = object ["topics" .= topics]


instance FromJSON TopicsResponse where
  parseJSON = withObject "TopicsResponse" $ \o -> do
    topics <- o .: "topics"
    return $ TopicsResponse topics
