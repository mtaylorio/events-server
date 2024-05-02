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
  , unStateUsers :: !(TVar (Map UUID [TVar Client]))
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
  modifyTVar' (unStateUsers state) $ modifyUsers client'
  modifyTVar' (unStateGroups state) $ modifyGroups client'
  modifyTVar' (unStateSessions state) $ delete (unClientSession client')
  where
  modifyUsers client' = adjust (Prelude.filter (/= client)) (unClientUser client')
  modifyGroups client' = adjust (Prelude.filter (/= client)) (unClientUser client')


joinGroup :: State -> UUID -> TVar Client -> STM ()
joinGroup state group client = do
  modifyTVar' (unStateGroups state) $ insertWith (++) group [client]


leaveGroup :: State -> UUID -> TVar Client -> STM ()
leaveGroup state group client = do
  modifyTVar' (unStateGroups state) $ adjust (Prelude.filter (/= client)) group
