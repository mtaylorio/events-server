module State
  ( State(..)
  , initState
  , insertClient
  , removeClient
  ) where

import Control.Concurrent.STM
import Data.Map.Strict
import Data.UUID (UUID)
import qualified Hasql.Pool as Pool
import qualified Servant.Client as SC

import Client
import Config
import Event
import Topic


data State = State
  { unStateConfig :: !Config
  , unStateClientEnv :: !SC.ClientEnv
  , unStateDatabase :: !Pool.Pool
  , unStateClients :: !(TVar (Map UUID (TVar Client)))
  , unStateTopics :: !(TopicManager UUID EventData)
  }


initState :: Config -> SC.ClientEnv -> Pool.Pool -> STM State
initState conf clientEnv db =
  State conf clientEnv db <$> newTVar empty <*> createTopicManager


insertClient :: State -> Client -> STM (TVar Client)
insertClient state client = do
  clientVar <- newTVar client
  modifyTVar' (unStateClients state) $ insert (unClientSession client) clientVar
  return clientVar


removeClient :: State -> TVar Client -> STM ()
removeClient state client = do
  client' <- readTVar client
  modifyTVar' (unStateClients state) $ delete (unClientSession client')
