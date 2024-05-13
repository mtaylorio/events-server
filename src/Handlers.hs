{-# LANGUAGE OverloadedStrings #-}
module Handlers
  ( module Handlers
  ) where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Time.Clock
import Data.UUID
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Servant
import qualified Data.Map as Map
import qualified Hasql.Pool as Pool
import qualified Network.WebSockets as WS
import qualified Servant.Client as SC

import IAM.Client
import IAM.GroupIdentifier
import IAM.UserIdentifier

import API
import Auth
import Client
import DB
import Event
import State
import Topic


handlePublish :: State -> UUID -> UUID -> EventData -> IO ()
handlePublish state topic _ = publish (unStateTopics state) topic


handleSubscribe :: State -> UUID -> TVar Client -> IO ()
handleSubscribe state topic client = do
  unsubscribe <- subscribe (unStateTopics state) topic handleEvent
  case unsubscribe of
    Just unsub -> atomically $ modifyTVar' client $ addSubscription topic unsub
    Nothing -> return ()
  where
  handleEvent :: EventData -> IO ()
  handleEvent eventData = do
    client' <- readTVarIO client
    WS.sendTextData (unClientConn client') (encode eventData)


handleUnsubscribe :: UUID -> TVar Client -> IO ()
handleUnsubscribe topic client = do
  client' <- readTVarIO client
  case filter ((== topic) . snd) $ unClientSubscriptions client' of
    [] -> return ()
    ((unsub, _):_) -> do
      unsub
      atomically $ modifyTVar' client $ removeSubscription topic


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


sessionsHandler :: State -> Auth -> Handler SessionsResponse
sessionsHandler state (Authenticated{}) = do
  clients <- liftIO $ readTVarIO $ unStateClients state
  return $ SessionsResponse $ Map.keys clients
sessionsHandler _ _ = throwError err401


sessionHandler :: State -> Auth -> UUID -> Handler SessionResponse
sessionHandler state (Authenticated{}) session = do
  clients <- liftIO $ readTVarIO $ unStateClients state
  case Map.lookup session clients of
    Just _ -> return $ SessionResponse session
    Nothing -> throwError err404
sessionHandler _ _ _ = throwError err401


createBroadcastTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createBroadcastTopicHandler state (Authenticated{}) = createDBTopicHandler state True
createBroadcastTopicHandler _ _ = \_ -> throwError err401


createSendReceiveTopicHandler :: State -> Auth -> UUID -> Handler NoContent
createSendReceiveTopicHandler state (Authenticated{}) = createDBTopicHandler state False
createSendReceiveTopicHandler _ _ = \_ -> throwError err401


createDBTopicHandler :: State -> Bool -> UUID -> Handler NoContent
createDBTopicHandler state broadcast topic = do
  createdAt <- liftIO getCurrentTime
  let db = unStateDatabase state
  let stmt = statement (DBTopic topic broadcast createdAt) insertTopic
  result <- liftIO $ Pool.use db $ transaction Serializable Read stmt
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Error inserting topic: " ++ show err
      throwError err500
    Right _ -> do
      if broadcast
        then liftIO $ atomically $ createBroadcastTopic (unStateTopics state) topic
        else liftIO $ atomically $ createSendReceiveTopic (unStateTopics state) topic
      return NoContent
