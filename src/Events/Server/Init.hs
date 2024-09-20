module Events.Server.Init
  ( initServer
  ) where

import Control.Concurrent.STM
import Hasql.Transaction
import Hasql.Transaction.Sessions
import qualified Hasql.Pool as Pool

import Events.DB
import Events.Topic
import Events.Server.State


initServer :: State -> IO ()
initServer state = do
  let db = unStateDatabase state
  result <- Pool.use db $ transaction Serializable Read $ statement () selectTopics
  case result of
    Left err -> do
      putStrLn $ "Error selecting topics: " ++ show err
    Right topics -> do
      mapM_ (initQueue state) topics


initQueue :: State -> DBTopic -> IO ()
initQueue state dbTopic =
  if dbTopicBroadcast dbTopic
    then atomically $ createBroadcastTopic (unStateTopics state) topicId
    else atomically $ createSendReceiveTopic (unStateTopics state) topicId
  where
    topicId = dbTopicId dbTopic
