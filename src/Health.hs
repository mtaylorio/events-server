{-# LANGUAGE OverloadedStrings #-}
module Health
  ( HealthResponse(..)
  , Status(..)
  ) where

import Data.Aeson


newtype HealthResponse = HealthResponse { status :: Status }

instance ToJSON HealthResponse where
  toJSON (HealthResponse s) = object ["status" .= s]


data Status
  = Healthy
  | Degraded
  | Unhealthy

instance ToJSON Status where
  toJSON Healthy = "Healthy"
  toJSON Degraded = "Degraded"
  toJSON Unhealthy = "Unhealthy"
