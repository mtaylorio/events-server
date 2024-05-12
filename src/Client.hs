{-# LANGUAGE OverloadedStrings #-}
module Client
  ( newClient
  , disconnected
  , addSubscription
  , removeSubscription
  , Client(..)
  , ClientHello(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Network.WebSockets as WS

import Topic


data Client = Client
  { unClientConn :: !WS.Connection
  , unClientUser :: !UUID
  , unClientGroups :: ![UUID]
  , unClientSession :: !UUID
  , unClientToken :: !Text
  , unClientSubscriptions :: ![(Unsubscribe, UUID)]
  }


data ClientHello = ClientHello
  { unClientHelloUser :: !UUID
  , unClientHelloSession :: !UUID
  , unClientHelloToken :: !Text
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
newClient conn (ClientHello user session token) = Client conn user [] session token []


disconnected :: Client -> IO ()
disconnected client = mapM_ fst $ unClientSubscriptions client


addSubscription :: UUID -> Unsubscribe -> Client -> Client
addSubscription topic unsub client = client
  { unClientSubscriptions = (unsub, topic) : unClientSubscriptions client }


removeSubscription :: UUID -> Client -> Client
removeSubscription topic client = client
  { unClientSubscriptions = filter ((/= topic) . snd) $ unClientSubscriptions client }
