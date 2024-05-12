{-# LANGUAGE OverloadedStrings #-}
module Handlers
  ( module Handlers
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.UUID
import Servant
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import qualified Servant.Client as SC

import IAM.Client
import IAM.GroupIdentifier
import IAM.UserIdentifier

import API
import Auth
import Client
import Event
import State
import Topic


handlePublish :: State -> UUID -> EventData -> IO ()
handlePublish state = publish (unStateTopics state)


handleSubscribe :: State -> UUID -> TVar Client -> IO ()
handleSubscribe state topic client = do
  unsubscribe <- subscribe (unStateTopics state) topic handleEvent
  case unsubscribe of
    Just unsubscribe' -> atomically $ modifyTVar' client $ addSubscription unsubscribe'
    Nothing -> return ()
  where
  handleEvent :: EventData -> IO ()
  handleEvent eventData = do
    client' <- readTVarIO client
    WS.sendTextData (unClientConn client') (encode eventData)


checkMembership :: State -> Client -> UUID -> IO ()
checkMembership state client group = do
  let uid = UserUUID $ unClientUser client
      uident = UserIdentifier (Just uid) Nothing Nothing
      gid = GroupUUID group
      gident = GroupId gid
      gclient = mkGroupClient gident
      mclient = memberClient gclient uident
      client' = getMembership mclient
  result <- SC.runClientM client' (unStateClientEnv state)
  case result of
    Right NoContent ->
      putStrLn "Membership check succeeded"
    Left err -> do
      throwIO $ userError $ "Membership check failed: " ++ show err


handleJoinGroup :: State -> TVar Client -> UUID -> IO ()
handleJoinGroup state client group = do
  client' <- readTVarIO client
  checkMembership state client' group
  atomically $ joinGroup state group client


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


sessionsHandler :: State -> Auth -> Handler SessionsResponse
sessionsHandler state (Authenticated{}) = do
  sessions <- liftIO $ readTVarIO $ unStateSessions state
  return $ SessionsResponse $ Map.keys sessions
sessionsHandler _ _ = throwError err401


sessionHandler :: State -> Auth -> UUID -> Handler SessionResponse
sessionHandler state (Authenticated{}) session = do
  sessions <- liftIO $ readTVarIO $ unStateSessions state
  case Map.lookup session sessions of
    Just client -> do
      client' <- liftIO $ readTVarIO client
      return $ SessionResponse session (unClientUser client') (unClientGroups client')
    Nothing -> throwError err404
sessionHandler _ _ _ = throwError err401


createBroadcastTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createBroadcastTopicHandler state (Authenticated{}) topic = do
  liftIO $ atomically $ createBroadcastTopic (unStateTopics state) topic
  return NoContent
createBroadcastTopicHandler _ _ _ = throwError err401


createSendReceiveTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createSendReceiveTopicHandler state (Authenticated{}) topic = do
  liftIO $ atomically $ createSendReceiveTopic (unStateTopics state) topic
  return NoContent
createSendReceiveTopicHandler _ _ _ = throwError err401
