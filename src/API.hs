{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module API
  ( module API
  ) where

import Data.Aeson
import Data.UUID
import Servant


data SessionResponse = SessionResponse
  { sessionResponseUser :: UUID
  , sessionResponseSession :: UUID
  , sessionResponseTopics :: [UUID]
  } deriving (Eq, Show)


instance ToJSON SessionResponse where
  toJSON (SessionResponse user session topics) =
    object ["user" .= user, "session" .= session, "topics" .= topics]


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o ->
    SessionResponse <$> o .: "user" <*> o .: "session" <*> o .: "topics"


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
