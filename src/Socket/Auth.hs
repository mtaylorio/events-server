{-# LANGUAGE OverloadedStrings #-}
module Socket.Auth
  ( module Socket.Auth
  ) where

import Control.Monad.IO.Class
import Data.Text
import Data.UUID
import System.IO
import qualified Servant.Client as SC

import IAM.Authorization
import IAM.Client
import IAM.Policy (Action(..), Effect(..))
import IAM.UserIdentifier

import Client
import Config
import State


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
  result <- liftIO $ SC.runClientM (authorizeClient authRequest) clientEnv
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

  clientEnv :: SC.ClientEnv
  clientEnv = unStateClientEnv state

  host :: Text
  host = configHost $ unStateConfig state

  token :: Text
  token = unClientToken client

  uident :: UserIdentifier
  uident = UserIdentifier (Just $ UserUUID $ unClientUser client) Nothing Nothing
