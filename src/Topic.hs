{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Topic
  ( createBroadcastTopic
  , createSendReceiveTopic
  , createTopicManager
  , publish
  , subscribe
  , EventHandler
  , TopicManager(..)
  , Unsubscribe
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Map.Strict
import Data.Maybe


type EventHandler e = e -> IO ()


type Unsubscribe = IO ()


class Topic' t e | t -> e where
  publish' :: t -> e -> IO ()
  subscribe' :: t -> EventHandler e -> IO Unsubscribe


data Topic e = forall t. Topic' t e => Topic t


instance Topic' (Topic e) e where
  publish' (Topic t) = publish' t
  subscribe' (Topic t) = subscribe' t


newtype TopicManager k e = TopicManager { unTopicManager :: TVar (Map k (Topic e)) }


createTopicManager :: STM (TopicManager k e)
createTopicManager = TopicManager <$> newTVar empty


createBroadcastTopic :: Ord k => TopicManager k e -> k -> STM (Topic e)
createBroadcastTopic tm k = do
  t <- createBroadcastTopic'
  insertTopic tm k t


createSendReceiveTopic :: Ord k => TopicManager k e -> k -> STM (Topic e)
createSendReceiveTopic tm k = do
  t <- createSendReceiveTopic'
  insertTopic tm k t


insertTopic :: (Ord k, Topic' t e) => TopicManager k e -> k -> t -> STM (Topic e)
insertTopic (TopicManager topics) k t = do
  modifyTVar' topics $ insert k (Topic t)
  return $ Topic t


lookupTopic :: Ord k => TopicManager k e -> k -> STM (Maybe (Topic e))
lookupTopic (TopicManager topics) k = do
  topics' <- readTVar topics
  return $ Data.Map.Strict.lookup k topics'


publish :: Ord k => TopicManager k e -> k -> e -> IO ()
publish tm k event = do
  mt <- atomically $ lookupTopic tm k
  case mt of
    Just t -> publish' t event
    Nothing -> return ()


subscribe :: Ord k => TopicManager k e -> k -> EventHandler e -> IO (Maybe Unsubscribe)
subscribe tm k listener = do
  mt <- atomically $ lookupTopic tm k
  case mt of
    Just t -> Just <$> subscribe' t listener
    Nothing -> return Nothing


newtype BroadcastTopic e = BroadcastTopic (TVar [(Int, EventHandler e)])


createBroadcastTopic' :: STM (BroadcastTopic e)
createBroadcastTopic' = BroadcastTopic <$> newTVar []


instance Topic' (BroadcastTopic e) e where
  publish' (BroadcastTopic listeners) event = do
    ls <- readTVarIO listeners
    forM_ ls $ \(_, listener) -> listener event
  subscribe' (BroadcastTopic listeners) listener = do
    lid <- atomically $ stateTVar listeners $ \ls ->
      let lid = maybe 0 (succ . fst) $ listToMaybe $ reverse ls
      in (lid, (lid, listener) : ls)
    return $ atomically $ modifyTVar' listeners $ Prelude.filter ((/= lid) . fst)


newtype SendReceiveTopic e = SendReceiveTopic (TChan e)


createSendReceiveTopic' :: STM (SendReceiveTopic e)
createSendReceiveTopic' = SendReceiveTopic <$> newBroadcastTChan


instance Topic' (SendReceiveTopic e) e where
  publish' (SendReceiveTopic chan) = atomically . writeTChan chan
  subscribe' (SendReceiveTopic chan) f = do
    listener <- atomically $ dupTChan chan
    threadId <- forkIO $ forever $ do
      event <- atomically $ readTChan listener
      f event
    return $ killThread threadId
