{-# LANGUAGE OverloadedStrings #-}
module Handlers
  ( module Handlers
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Data.UUID
import qualified Servant.Client as SC

import IAM.Authorization
import IAM.Client
import IAM.Policy (Action(Read))
import IAM.UserIdentifier

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
    Left err -> do
      putStrLn $ "Authorization failed: " ++ show err
      throwIO $ userError "Authorization failed"
    Right _ -> putStrLn "Authorization succeeded"


handleJoinGroup :: State -> TVar Client -> UUID -> IO ()
handleJoinGroup state client group = do
  client' <- readTVarIO client
  authorizeJoinGroup state client' group
  atomically $ joinGroup state group client
