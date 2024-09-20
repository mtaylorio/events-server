{-# LANGUAGE OverloadedStrings #-}
module Events.Socket.Auth
  ( module Events.Socket.Auth
  ) where

import Control.Monad.IO.Class
import Data.Text
import Data.UUID
import System.IO

import IAM.Authorization
import IAM.Client
import IAM.Policy (Action(..), Effect(..))
import IAM.UserIdentifier

import Events.Config
import Events.Server.Client
import Events.Server.State


authorizeTopic :: State -> Client -> Action -> UUID -> IO Bool
authorizeTopic state client action topicId =
  authorizeTopicResource state client action $
    "/topics/" <> toText topicId


authorizeTopicEvent :: State -> Client -> Action -> UUID -> UUID -> IO Bool
authorizeTopicEvent state client action topicId eventId =
  authorizeTopicResource state client action $
    "/topics/" <> toText topicId <> "/events/" <> toText eventId


authorizeTopicResource :: State -> Client -> Action -> Text -> IO Bool
authorizeTopicResource state client action resource = do
  result <- liftIO $ iamRequest (unStateIAMClient state) (authorizeClient authRequest)
  case result of
    Left err -> do
      hPutStrLn stderr $ "Authorization failed: " <> show err
      hFlush stderr
      return False
    Right (AuthorizationResponse Allow) -> return True
    Right (AuthorizationResponse Deny) -> return False

  where

  authRequest :: AuthorizationRequest
  authRequest = AuthorizationRequest
    { authorizationRequestUser = uident
    , authorizationRequestHost = host
    , authorizationRequestAction = action
    , authorizationRequestResource = resource
    , authorizationRequestToken = Just token
    }

  host :: Text
  host = configHost $ unStateConfig state

  token :: Text
  token = unClientToken client

  uident :: UserIdentifier
  uident = UserIdentifier (Just $ UserUUID $ unClientUser client) Nothing Nothing
