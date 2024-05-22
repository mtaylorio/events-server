{-# LANGUAGE OverloadedStrings #-}
module API.Topics
  ( module API.Topics
  ) where

import Data.Aeson
import Data.UUID
import Data.Time.Clock


data TopicInfo = TopicInfo
  { topicInfoId :: UUID
  , topicInfoBroadcast :: Bool
  , topicInfoLogEvents :: Bool
  , topicInfoCreatedAt :: UTCTime
  } deriving (Eq, Show)


instance ToJSON TopicInfo where
  toJSON (TopicInfo id' broadcast logEvents createdAt) = object
    ["id" .= id'
    , "broadcast" .= broadcast
    , "logEvents" .= logEvents
    , "createdAt" .= createdAt
    ]


instance FromJSON TopicInfo where
  parseJSON = withObject "TopicInfo" $ \o -> TopicInfo
    <$> o .: "id"
    <*> o .: "broadcast"
    <*> o .: "logEvents"
    <*> o .: "createdAt"


newtype TopicsResponse
  = TopicsResponse { unTopicsResponseTopics :: [TopicInfo] }
  deriving (Eq, Show)


instance ToJSON TopicsResponse where
  toJSON (TopicsResponse topics) = object ["topics" .= topics]


instance FromJSON TopicsResponse where
  parseJSON = withObject "TopicsResponse" $ \o -> do
    topics <- o .: "topics"
    return $ TopicsResponse topics
