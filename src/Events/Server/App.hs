{-# LANGUAGE DataKinds #-}
module Events.Server.App
  ( app
  ) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Servant
import Servant.Server.Experimental.Auth
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Events.API
import Events.Config
import Events.Server.API
import Events.Server.Auth
import Events.Server.State
import Events.Socket (websocketHandler)


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
    authContext' = authContext configHost' (unStateIAMClient state)
    configHost' = configHost $ unStateConfig state
    handler = websocketHandler state
