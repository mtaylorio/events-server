{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API
  ( module API
  ) where

import Data.Aeson
import Data.UUID
import Servant


newtype SessionResponse = SessionResponse
  { sessionResponseSession :: UUID
  } deriving (Eq, Show)


instance ToJSON SessionResponse where
  toJSON (SessionResponse session) = object ["session" .= session]


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o -> do
    session <- o .: "session"
    return $ SessionResponse session


newtype SessionsResponse
  = SessionsResponse { unSessionsResponseSessions :: [UUID] }
  deriving (Eq, Show)


instance ToJSON SessionsResponse where
  toJSON (SessionsResponse sessions) = object ["sessions" .= sessions]


instance FromJSON SessionsResponse where
  parseJSON = withObject "SessionsResponse" $ \o -> do
    sessions <- o .: "sessions"
    return $ SessionsResponse sessions


type API = AuthProtect "signature-auth" :>
  ( "sessions" :> Get '[JSON] SessionsResponse
  :<|> "session" :> Capture "session" UUID :> Get '[JSON] SessionResponse
  :<|> "topics" :> TopicsAPI
  )


type TopicsAPI = Capture "topic" UUID :>
  ( "broadcast" :> PutNoContent
  :<|> "send-receive" :> PutNoContent
  )
