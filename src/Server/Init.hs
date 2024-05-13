module Server.Init
  ( initServer
  ) where

import Control.Concurrent.STM
import Hasql.Transaction
import Hasql.Transaction.Sessions
import qualified Hasql.Pool as Pool

import DB
import State
import Topic


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
initQueue state (DBTopic topicId True _) =
  atomically $ createBroadcastTopic (unStateTopics state) topicId
initQueue state (DBTopic topicId False _) =
  atomically $ createSendReceiveTopic (unStateTopics state) topicId
