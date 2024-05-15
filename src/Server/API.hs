module Server.API
  ( module Server.API
  ) where

import Servant

import API
import Server.Auth
import Server.Handlers
import State


server :: State -> Server API
server state auth
  = sessionsHandler state auth
  :<|> sessionHandler state auth
  :<|> topicsServer state auth


topicsServer :: State -> Auth -> Server TopicsAPI
topicsServer state auth topic
  = createBroadcastTopicHandler state auth topic
  :<|> createSendReceiveTopicHandler state auth topic
  :<|> eventsHandlers
  where
  eventsHandlers
    = logEventsHandler state auth topic
    :<|> deleteLogEventsHandler state auth topic
