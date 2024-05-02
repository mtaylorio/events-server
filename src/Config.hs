{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( module Config
  ) where

import Data.Text
import System.Environment (lookupEnv)


getHost :: IO Text
getHost = lookupEnv "HOST" >>= \case
  Just h -> return $ pack h
  Nothing -> return "localhost"
