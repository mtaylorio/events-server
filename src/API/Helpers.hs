{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module API.Helpers
  ( module API.Helpers
  ) where

import Data.Aeson
import Servant


type ListAPI a
  = QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] (ListResponse a)


data ListResponse a = ListResponse
  { listResponseItems :: [a]
  , listResponseTotal :: Int
  } deriving (Eq, Show)


instance ToJSON a => ToJSON (ListResponse a) where
  toJSON (ListResponse items total) =
    object ["items" .= items, "total" .= total]


instance FromJSON a => FromJSON (ListResponse a) where
  parseJSON = withObject "ListResponse" $ \o ->
    ListResponse <$> o .: "items" <*> o .: "total"
