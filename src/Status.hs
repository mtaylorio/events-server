{-# LANGUAGE OverloadedStrings #-}
module Status
  ( Health(..)
  , StatusResponse(..)
  ) where

import Data.Aeson
import Data.Text (Text)


data StatusResponse = StatusResponse
  { status :: Health
  , version :: Text
  }

instance ToJSON StatusResponse where
  toJSON (StatusResponse s v) = object ["status" .= s, "version" .= v]


data Health
  = Healthy
  | Degraded
  | Unhealthy

instance ToJSON Health where
  toJSON Healthy = "Healthy"
  toJSON Degraded = "Degraded"
  toJSON Unhealthy = "Unhealthy"
