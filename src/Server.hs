{-# LANGUAGE OverloadedStrings #-}
module Server
  ( runServer
  ) where

import Control.Concurrent.STM (atomically)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Socket (websocketHandler)
import State (initState, State)


app :: State -> Wai.Application
app state req respond =
  let handler = websocketHandler state
   in case WaiWS.websocketsApp WS.defaultConnectionOptions handler req of
    Nothing -> respond $ Wai.responseLBS HTTP.status400 [] "Not a WebSocket request"
    Just response -> respond response


runServer :: IO ()
runServer = do
  state <- atomically initState
  putStrLn "Starting server on http://localhost:8080"
  Warp.run 8080 $ app state
