{-# LANGUAGE OverloadedStrings #-}
module Socket
  ( websocketHandler
  ) where

import Control.Concurrent.STM
import Control.Exception (catch, throwIO)
import Data.Aeson
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.UUID (UUID)
import System.IO
import qualified Data.Map.Strict as M
import qualified Network.WebSockets as WS
import qualified Servant.Client as SC

import IAM.Authorization
import IAM.Client
import IAM.Policy (Action(Read), Effect(Allow, Deny))
import IAM.UserIdentifier

import Client
import Event
import Message
import Recipient
import State


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
    atomically $ removeClient state client
  onException client WS.ConnectionClosed = do
    atomically $ removeClient state client
  onException client e = do
    atomically $ removeClient state client
    putStrLn $ "Exception: " ++ show (e :: WS.ConnectionException)
    hFlush stdout


websocketHandshake :: State -> WS.PendingConnection -> IO (TVar Client, WS.Connection)
websocketHandshake state pending = do
  conn <- WS.acceptRequest pending
  clientHelloBytes <- WS.receiveData conn
  case decode clientHelloBytes of
    Just clientHello -> do
      let uid = UserUUID $ unClientHelloUser clientHello
          uident = UserIdentifier (Just uid) Nothing Nothing
          client = authorizeClient $ AuthorizationRequest
            { authorizationRequestUser = uident
            , authorizationRequestHost = unStateHost state
            , authorizationRequestAction = Read
            , authorizationRequestResource = "/"
            , authorizationRequestToken = Just $ unClientHelloToken clientHello
            }
      result <- SC.runClientM client (unStateClientEnv state)
      case result of
        Right (AuthorizationResponse Allow) -> do
          client' <- atomically $ insertClient state $ newClient conn clientHello
          return (client', conn)
        Right (AuthorizationResponse Deny) -> do
          WS.sendClose conn ("Authorization denied" :: Text)
          throwIO $ userError "Authorization denied"
        Left err -> do
          putStrLn $ "Authorization failed: " ++ show err
          hFlush stdout
          WS.sendClose conn ("Authorization failed" :: Text)
          throwIO $ userError "Authorization failed"
    Nothing ->
      throwIO $ userError "Expected a ClientHello message"


websocketLoop :: State -> TVar Client -> WS.Connection -> IO ()
websocketLoop state client conn = do
  evtBytes <- WS.receiveData conn
  case decode evtBytes of
    Just evt ->
      case evt of
        EventJoinGroup group -> do
          atomically $ joinGroup state group client
        EventLeaveGroup group -> do
          atomically $ leaveGroup state group client
        EventMessage msg -> do
          case unMessageRecipient msg of
            UserRecipient user -> do
              sendToUser state user msg
            GroupRecipient group -> do
              sendToGroup state group msg
            SessionRecipient session -> do
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
