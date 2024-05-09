{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API
  ( module API
  ) where

import Data.Aeson
import Data.UUID
import Servant


newtype SessionsResponse
  = SessionsResponse { unSessionsResponseSessions :: [UUID] }
  deriving (Eq, Show)


instance ToJSON SessionsResponse where
  toJSON (SessionsResponse sessions) = object ["sessions" .= sessions]


instance FromJSON SessionsResponse where
  parseJSON = withObject "SessionsResponse" $ \o -> do
    sessions <- o .: "sessions"
    return $ SessionsResponse sessions


newtype UsersResponse
  = UsersResponse { unUsersResponseUsers :: [UUID] }
  deriving (Eq, Show)


instance ToJSON UsersResponse where
  toJSON (UsersResponse users) = object ["users" .= users]


instance FromJSON UsersResponse where
  parseJSON = withObject "UsersResponse" $ \o -> do
    users <- o .: "users"
    return $ UsersResponse users


newtype GroupsResponse
  = GroupsResponse { unGroupsResponseGroups :: [UUID] }
  deriving (Eq, Show)


instance ToJSON GroupsResponse where
  toJSON (GroupsResponse groups) = object ["groups" .= groups]


instance FromJSON GroupsResponse where
  parseJSON = withObject "GroupsResponse" $ \o -> do
    groups <- o .: "groups"
    return $ GroupsResponse groups


type API = AuthProtect "signature-auth" :>
  ( "sessions" :> Get '[JSON] SessionsResponse
  :<|> "users" :> Get '[JSON] UsersResponse
  :<|> "groups" :> Get '[JSON] GroupsResponse
  )
