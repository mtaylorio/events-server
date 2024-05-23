module Server.API
  ( module Server.API
  ) where

import Data.UUID
import Servant

import API
import Server.Auth
import Server.Handlers
import State


server :: State -> Server API
server state auth
  = sessionsServer state auth
  :<|> topicsServer state auth


sessionsServer :: State -> Auth -> Server SessionsAPI
sessionsServer state auth
  = sessionsHandler state auth
  :<|> sessionHandler state auth


topicsServer :: State -> Auth -> Server TopicsAPI
topicsServer state auth
  = topicsHandler state auth
  :<|> createTopicHandler state auth
  :<|> topicServer state auth


topicServer :: State -> Auth -> UUID -> Server TopicAPI
topicServer state auth topic
  = topicHandler state auth topic
  :<|> deleteTopicHandler state auth topic
  :<|> updateTopicHandler state auth topic
