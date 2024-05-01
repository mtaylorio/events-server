{-# LANGUAGE TupleSections #-}
module State
  ( State(..)
  , initState
  , insertClient
  , joinGroup
  , leaveGroup
  ) where

import Control.Concurrent.STM
import Data.Map.Strict
import Data.UUID (UUID)

import Client


data State = State
  { unStateUsers :: TVar (Map UUID [TVar Client])
  , unStateGroups :: TVar (Map UUID [TVar Client])
  , unStateSessions :: TVar (Map UUID (TVar Client))
  }


initState :: STM State
initState = State <$> newTVar empty <*> newTVar empty <*> newTVar empty


insertClient :: State -> Client -> STM (TVar Client)
insertClient state client = do
  clientVar <- newTVar client
  modifyTVar' (unStateUsers state) $ insertWith (++) (unClientUser client) [clientVar]
  modifyTVar' (unStateGroups state) $ unionWith (++) (modifyGroups clientVar)
  modifyTVar' (unStateSessions state) $ insert (unClientSession client) clientVar
  return clientVar
  where
  modifyGroups clientVar = fromList $ fmap (, [clientVar]) (unClientGroups client)


joinGroup :: State -> UUID -> TVar Client -> STM ()
joinGroup state group client = do
  modifyTVar' (unStateGroups state) $ insertWith (++) group [client]


leaveGroup :: State -> UUID -> TVar Client -> STM ()
leaveGroup state group client = do
  modifyTVar' (unStateGroups state) $ adjust (Prelude.filter (/= client)) group
