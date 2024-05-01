{-# LANGUAGE OverloadedStrings #-}
module Client
  ( Client(..)
  , ClientHello(..)
  , newClient
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Network.WebSockets as WS


data Client = Client
  { unClientConn :: WS.Connection
  , unClientUser :: UUID
  , unClientGroups :: [UUID]
  , unClientSession :: UUID
  , unClientToken :: Text
  }


data ClientHello = ClientHello
  { unClientHelloUser :: UUID
  , unClientHelloSession :: UUID
  , unClientHelloToken :: Text
  } deriving (Eq, Show)


instance FromJSON ClientHello where
  parseJSON (Object o) = ClientHello
    <$> o .: "user"
    <*> o .: "session"
    <*> o .: "token"
  parseJSON _ = fail "Expected an object"


instance ToJSON ClientHello where
  toJSON (ClientHello user session token) =
    object ["user" .= user, "session" .= session, "token" .= token]


newClient :: WS.Connection -> ClientHello -> Client
newClient conn (ClientHello user session token) = Client conn user [] session token
