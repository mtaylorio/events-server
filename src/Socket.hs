module Socket
  ( websocketHandler
  ) where

import Control.Concurrent.STM
import Control.Exception (catch, throwIO)
import Data.Aeson
import Data.Foldable (forM_)
import Data.UUID (UUID)
import qualified Data.Map.Strict as M
import qualified Network.WebSockets as WS

import Client
import Event
import Message
import Recipient
import State


websocketHandler :: State -> WS.PendingConnection -> IO ()
websocketHandler state pending = do
  (client, conn) <- websocketHandshake state pending
  catch (websocketLoop state client conn) (onException client)
  where
  onException client e = do
    putStrLn $ "Exception: " ++ show (e :: WS.ConnectionException)
    atomically $ removeClient state client


websocketHandshake :: State -> WS.PendingConnection -> IO (TVar Client, WS.Connection)
websocketHandshake state pending = do
  conn <- WS.acceptRequest pending
  putStrLn "WebSocket connection established"
  clientHelloBytes <- WS.receiveData conn
  case decode clientHelloBytes of
    Just clientHello -> do
      client <- atomically $ insertClient state $ newClient conn clientHello
      return (client, conn)
    Nothing ->
      throwIO $ userError "Expected a ClientHello message"


websocketLoop :: State -> TVar Client -> WS.Connection -> IO ()
websocketLoop state client conn = do
  evtBytes <- WS.receiveData conn
  case decode evtBytes of
    Just evt ->
      case evt of
        EventJoinGroup group -> do
          putStrLn $ "Joining group " ++ show group
          atomically $ joinGroup state group client
        EventLeaveGroup group -> do
          putStrLn $ "Leaving group " ++ show group
          atomically $ leaveGroup state group client
        EventMessage msg -> do
          putStrLn "Received a message"
          case unMessageRecipient msg of
            UserRecipient user -> do
              putStrLn $ "Sending message to user " ++ show user
              sendToUser state user msg
            GroupRecipient group -> do
              putStrLn $ "Sending message to group " ++ show group
              sendToGroup state group msg
            SessionRecipient session -> do
              putStrLn $ "Sending message to session " ++ show session
              sendToSession state session msg
    Nothing ->
      throwIO $ userError "Expected a message"
  websocketLoop state client conn


sendToClient :: Message -> TVar Client -> IO ()
sendToClient msg clientVar = do
  client <- readTVarIO clientVar
  WS.sendTextData (unClientConn client) (encode msg)


sendToUser :: State -> UUID -> Message -> IO ()
sendToUser state user msg = do
  users <- readTVarIO $ unStateUsers state
  forM_ (M.lookup user users) (mapM_ (sendToClient msg))


sendToGroup :: State -> UUID -> Message -> IO ()
sendToGroup state group msg = do
  groups <- readTVarIO $ unStateGroups state
  forM_ (M.lookup group groups) (mapM_ (sendToClient msg))


sendToSession :: State -> UUID -> Message -> IO ()
sendToSession state session msg = do
  sessions <- readTVarIO $ unStateSessions state
  forM_ (M.lookup session sessions) (sendToClient msg)
