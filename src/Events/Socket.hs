{-# LANGUAGE OverloadedStrings #-}
module Events.Socket
  ( websocketHandler
  ) where

import Control.Concurrent.STM
import Control.Exception (catch, throwIO)
import Data.Aeson
import System.IO
import qualified Network.WebSockets as WS

import Events.Event
import Events.Server.Client
import Events.Server.State
import Events.Socket.Handlers


websocketHandler :: State -> WS.PendingConnection -> IO ()
websocketHandler state pending = do
  (client, conn) <- websocketHandshake state pending
  catch (handleMessages client conn) (onException client)
  where
  handleMessages :: TVar Client -> WS.Connection -> IO ()
  handleMessages client conn = do
    WS.withPingThread conn 30 (return ()) $ websocketLoop state client conn
    atomically $ removeClient state client
  onException :: TVar Client -> WS.ConnectionException -> IO ()
  onException client (WS.CloseRequest _ _) = do
    disconnected =<< readTVarIO client
    atomically $ removeClient state client
  onException client WS.ConnectionClosed = do
    disconnected =<< readTVarIO client
    atomically $ removeClient state client
  onException client e = do
    disconnected =<< readTVarIO client
    atomically $ removeClient state client
    putStrLn $ "Exception: " ++ show (e :: WS.ConnectionException)
    hFlush stdout


websocketHandshake :: State -> WS.PendingConnection -> IO (TVar Client, WS.Connection)
websocketHandshake state pending = do
  conn <- WS.acceptRequest pending
  clientHelloBytes <- WS.receiveData conn
  case decode clientHelloBytes of
    Just clientHello -> do
      client' <- atomically $ insertClient state $ newClient conn clientHello
      return (client', conn)
    Nothing ->
      throwIO $ userError "Expected a ClientHello message"


websocketLoop :: State -> TVar Client -> WS.Connection -> IO ()
websocketLoop state client conn = do
  evtBytes <- WS.receiveData conn
  case decode evtBytes of
    Just evt ->
      case evt of
        EventPublish data' ->
          handlePublish state client $ EventWrapper evtBytes data'
        EventSubscribe topic ->
          handleSubscribe state topic client
        EventUnsubscribe topic ->
          handleUnsubscribe topic client
        EventReplay topic ->
          handleReplay state topic client
    Nothing ->
      throwIO $ userError "Expected a message"
  websocketLoop state client conn
