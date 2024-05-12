module Server.App
  ( app
  ) where

import Network.Wai.Middleware.RequestLogger
import Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import API
import Auth
import Config
import Server.API
import Socket (websocketHandler)
import State


app :: State -> Wai.Application
app state req respond' =
  let handler = websocketHandler state
   in case WaiWS.websocketsApp WS.defaultConnectionOptions handler req of
    Just response -> respond' response
    Nothing -> logStdout (
      serveWithContext
      (Proxy :: Proxy API)
      (authContext (configHost $ unStateConfig state) (unStateClientEnv state))
      (server state)
      ) req respond'
