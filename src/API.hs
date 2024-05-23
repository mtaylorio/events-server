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
  :<|> ReqBody '[JSON] CreateTopic :> Post '[JSON] TopicResponse
  :<|> Capture "topic" UUID :> TopicAPI
    )


type TopicAPI
  = ( Get '[JSON] TopicResponse
  :<|> DeleteNoContent
  :<|> "broadcast" :> PostNoContent
  :<|> "send-receive" :> PostNoContent
  :<|> "log-events" :> ( PostNoContent :<|> DeleteNoContent )
    )
