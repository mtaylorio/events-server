{-# LANGUAGE OverloadedStrings #-}
module Handlers
  ( module Handlers
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.UUID
import Servant
import qualified Data.Map as Map
import qualified Servant.Client as SC

import IAM.Authorization
import IAM.Client
import IAM.Policy (Action(Read), Effect(Allow, Deny))
import IAM.UserIdentifier

import API
import Auth
import Client
import State


authorizeJoinGroup :: State -> Client -> UUID -> IO ()
authorizeJoinGroup state client group = do
  let uid = UserUUID $ unClientUser client
      uident = UserIdentifier (Just uid) Nothing Nothing
      auth = AuthorizationRequest
        { authorizationRequestUser = uident
        , authorizationRequestHost = unStateHost state
        , authorizationRequestAction = Read
        , authorizationRequestResource = "/groups/" <> toText group
        , authorizationRequestToken = Just $ unClientToken client
        }
  result <- SC.runClientM (authorizeClient auth) (unStateClientEnv state)
  case result of
    Right (AuthorizationResponse Allow) ->
      putStrLn "Authorization succeeded"
    Right (AuthorizationResponse Deny) -> do
      putStrLn "Authorization denied"
      throwIO $ userError "Authorization denied"
    Left err -> do
      putStrLn $ "Authorization failed: " ++ show err
      throwIO $ userError "Authorization failed"


handleJoinGroup :: State -> TVar Client -> UUID -> IO ()
handleJoinGroup state client group = do
  client' <- readTVarIO client
  authorizeJoinGroup state client' group
  atomically $ joinGroup state group client


sessionsHandler :: State -> Auth -> Handler SessionsResponse
sessionsHandler state (Authenticated{}) = do
  sessions <- liftIO $ readTVarIO $ unStateSessions state
  return $ SessionsResponse $ Map.keys sessions
sessionsHandler _ _ = throwError err401


usersHandler :: State -> Auth -> Handler UsersResponse
usersHandler state (Authenticated{}) = do
  users <- liftIO $ readTVarIO $ unStateUsers state
  return $ UsersResponse $ Map.keys users
usersHandler _ _ = throwError err401


groupsHandler :: State -> Auth -> Handler GroupsResponse
groupsHandler state (Authenticated{}) = do
  groups <- liftIO $ readTVarIO $ unStateGroups state
  return $ GroupsResponse $ Map.keys groups
groupsHandler _ _ = throwError err401
