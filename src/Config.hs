{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( module Config
  ) where

import Data.Text
import System.Environment (lookupEnv)


data Config = Config
  { configHost :: Text
  , configOpts :: ConfigOpts
  , configPostgres :: ConfigPostgres
  } deriving (Show)


data ConfigOpts = ConfigOpts
  { configOptsPort :: Int
  , configOptsMigrations :: FilePath
  } deriving (Show)


data ConfigPostgres = ConfigPostgres
  { configPostgresHost :: Text
  , configPostgresPort :: Int
  , configPostgresUser :: Text
  , configPostgresPass :: Text
  , configPostgresDb :: Text
  } deriving (Show)


getHost :: IO Text
getHost = loadEnvWithDefault "localhost" "HOST" pack


getConfig :: ConfigOpts -> IO Config
getConfig opts = Config
  <$> getHost
  <*> return opts
  <*> getPostgresConfig


getPostgresConfig :: IO ConfigPostgres
getPostgresConfig = ConfigPostgres
  <$> loadEnvWithDefault "localhost" "POSTGRES_HOST" pack
  <*> loadEnvWithDefault 5432 "POSTGRES_PORT" read
  <*> loadEnvWithDefault "postgres" "POSTGRES_USER" pack
  <*> loadEnvWithDefault "postgres" "POSTGRES_PASSWORD" pack
  <*> loadEnvWithDefault "events" "POSTGRES_DB" pack


loadEnvWithDefault :: a -> String -> (String -> a) -> IO a
loadEnvWithDefault def env f = lookupEnv env >>= \case
  Just v -> return $ f v
  Nothing -> return def
