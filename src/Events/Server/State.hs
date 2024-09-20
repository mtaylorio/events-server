module Events.Server.State
  ( State(..)
  , initState
  , insertClient
  , removeClient
  , deleteTopicFromState
  ) where

import Control.Concurrent.STM
import Data.Map.Strict
import Data.UUID (UUID)
import qualified Hasql.Pool as Pool

import IAM.Client (IAMClient)

import Events.Config
import Events.Event
import Events.Server.Client
import Events.Topic


data State = State
  { unStateConfig :: !Config
  , unStateIAMClient :: !IAMClient
  , unStateDatabase :: !Pool.Pool
  , unStateClients :: !(TVar (Map UUID (TVar Client)))
  , unStateTopics :: !(TopicManager UUID EventWrapper)
  }


initState :: Config -> IAMClient -> Pool.Pool -> STM State
initState conf iamClient db =
  State conf iamClient db <$> newTVar empty <*> createTopicManager


insertClient :: State -> Client -> STM (TVar Client)
insertClient state client = do
  clientVar <- newTVar client
  modifyTVar' (unStateClients state) $ insert (unClientSession client) clientVar
  return clientVar


removeClient :: State -> TVar Client -> STM ()
removeClient state client = do
  client' <- readTVar client
  modifyTVar' (unStateClients state) $ delete (unClientSession client')


deleteTopicFromState :: State -> UUID -> STM ()
deleteTopicFromState state topicId = do
  removeTopic (unStateTopics state) topicId
  return ()
