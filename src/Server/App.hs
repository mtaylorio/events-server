{-# LANGUAGE DataKinds #-}
module Server.App
  ( app
  ) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Server.Experimental.Auth
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import API
import Config
import Server.API
import Server.Auth
import Socket (websocketHandler)
import State


app :: State -> Wai.Application
app state req respond' =
  case WaiWS.websocketsApp WS.defaultConnectionOptions handler req of
    Just response -> respond' response
    Nothing -> logStdout application req respond'
  where
    apiProxy :: Proxy API
    apiProxy = Proxy
    application :: Wai.Application
    application = serveWithContext apiProxy authContext' (server state)
    authContext' :: Context '[AuthHandler Request Auth]
    authContext' = authContext configHost' (unStateClientEnv state)
    configHost' = configHost $ unStateConfig state
    handler = websocketHandler state
