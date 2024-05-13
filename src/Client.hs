{-# LANGUAGE OverloadedStrings #-}
module Client
  ( newClient
  , disconnected
  , addSubscription
  , removeSubscription
  , listSubscriptions
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
  , unClientToken :: !Text
  , unClientSession :: !UUID
  , unClientSubscriptions :: ![(Unsubscribe, UUID)]
  }


data ClientHello = ClientHello
  { unClientHelloUser :: !UUID
  , unClientHelloToken :: !Text
  , unClientHelloSession :: !UUID
  } deriving (Eq, Show)


instance FromJSON ClientHello where
  parseJSON (Object o) = ClientHello
    <$> o .: "user"
    <*> o .: "token"
    <*> o .: "session"
  parseJSON _ = fail "Expected an object"


instance ToJSON ClientHello where
  toJSON (ClientHello user token session) =
    object ["user" .= user, "token" .= token, "session" .= session]


newClient :: WS.Connection -> ClientHello -> Client
newClient conn (ClientHello user token session) = Client conn user token session []


disconnected :: Client -> IO ()
disconnected client = mapM_ fst $ unClientSubscriptions client


listSubscriptions :: Client -> [UUID]
listSubscriptions = map snd . unClientSubscriptions


addSubscription :: UUID -> Unsubscribe -> Client -> Client
addSubscription topic unsub client = client
  { unClientSubscriptions = (unsub, topic) : unClientSubscriptions client }


removeSubscription :: UUID -> Client -> Client
removeSubscription topic client = client
  { unClientSubscriptions = filter ((/= topic) . snd) $ unClientSubscriptions client }
