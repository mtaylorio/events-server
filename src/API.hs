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


type API
  = AuthProtect "signature-auth" :>
    ( "sessions" :> SessionsAPI
  :<|> "topics" :> TopicsAPI
    )


type SessionsAPI
  = ( Get '[JSON] SessionsResponse
  :<|> Capture "session" UUID :> SessionAPI
    )


type SessionAPI
  = Get '[JSON] SessionResponse


type TopicsAPI
  = ( Get '[JSON] TopicsResponse
  :<|> ReqBody '[JSON] CreateTopic :> Post '[JSON] TopicResponse
  :<|> Capture "topic" UUID :> TopicAPI
    )


type TopicAPI
  = ( Get '[JSON] TopicResponse
  :<|> DeleteNoContent
  :<|> ReqBody '[JSON] UpdateTopic :> Put '[JSON] TopicResponse
    )
