{-# LANGUAGE OverloadedStrings #-}
module Client
  ( Client(..)
  , ClientHello(..)
  , newClient
  ) where

import Data.Aeson
import Data.UUID (UUID)
import qualified Network.WebSockets as WS


data Client = Client
  { unClientConn :: WS.Connection
  , unClientUser :: UUID
  , unClientGroups :: [UUID]
  , unClientSession :: UUID
  }


data ClientHello = ClientHello
  { unClientHelloUser :: UUID
  , unClientHelloSession :: UUID
  } deriving (Eq, Show)


instance FromJSON ClientHello where
  parseJSON (Object o) = ClientHello <$> o .: "user" <*> o .: "session"
  parseJSON _ = fail "Expected an object"


instance ToJSON ClientHello where
  toJSON (ClientHello user session) = object ["user" .= user, "session" .= session]


newClient :: WS.Connection -> ClientHello -> Client
newClient conn (ClientHello user session) = Client conn user [] session
