{-# LANGUAGE TupleSections #-}
module State
  ( State(..)
  , initState
  , insertClient
  , removeClient
  , joinGroup
  , leaveGroup
  ) where

import Control.Concurrent.STM
import Data.Map.Strict
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Servant.Client as SC

import Client


data State = State
  { unStateHost :: !Text
  , unStateClientEnv :: !SC.ClientEnv
  , unStateUsers :: !(TVar (Map Text [TVar Client]))
  , unStateGroups :: !(TVar (Map UUID [TVar Client]))
  , unStateSessions :: !(TVar (Map UUID (TVar Client)))
  }


initState :: Text -> SC.ClientEnv -> STM State
initState host clientEnv =
  State host clientEnv <$> newTVar empty <*> newTVar empty <*> newTVar empty


insertClient :: State -> Client -> STM (TVar Client)
insertClient state client = do
  clientVar <- newTVar client
  modifyTVar' (unStateUsers state) $ insertWith (++) (unClientUser client) [clientVar]
  modifyTVar' (unStateGroups state) $ unionWith (++) (modifyGroups clientVar)
  modifyTVar' (unStateSessions state) $ insert (unClientSession client) clientVar
  return clientVar
  where
  modifyGroups clientVar = fromList $ fmap (, [clientVar]) (unClientGroups client)


removeClient :: State -> TVar Client -> STM ()
removeClient state client = do
  client' <- readTVar client
  modifyTVar' (unStateUsers state) modifyUsers
  modifyTVar' (unStateGroups state) modifyGroups
  modifyTVar' (unStateSessions state) $ delete (unClientSession client')
  where
  modifyUsers :: Map Text [TVar Client] -> Map Text [TVar Client]
  modifyUsers = Data.Map.Strict.map f where f = Prelude.filter (/= client)
  modifyGroups :: Map UUID [TVar Client] -> Map UUID [TVar Client]
  modifyGroups = Data.Map.Strict.map f where f = Prelude.filter (/= client)


joinGroup :: State -> UUID -> TVar Client -> STM ()
joinGroup state group client = do
  modifyTVar' (unStateGroups state) $ insertWith (++) group [client]


leaveGroup :: State -> UUID -> TVar Client -> STM ()
leaveGroup state group client = do
  modifyTVar' (unStateGroups state) $ adjust (Prelude.filter (/= client)) group
