{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API
  ( module API
  , module API.Sessions
  , module API.Topics
  ) where

import Data.UUID
import Servant

import API.Sessions
import API.Topics


type API = AuthProtect "signature-auth" :>
  ( "sessions" :> Get '[JSON] SessionsResponse
  :<|> "session" :> Capture "session" UUID :> Get '[JSON] SessionResponse
  :<|> "topics" :> TopicsAPI
  )


type TopicsAPI
  = ( Get '[JSON] TopicsResponse
  :<|> ReqBody '[JSON] TopicInfo :> PostNoContent
  :<|> Capture "topic" UUID :> TopicAPI
    )


type TopicAPI
  = ( "broadcast" :> PostNoContent
  :<|> "send-receive" :> PostNoContent
  :<|> "log-events" :> ( PostNoContent :<|> DeleteNoContent )
    )
