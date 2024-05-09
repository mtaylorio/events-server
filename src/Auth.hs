{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Auth
  ( module Auth
  ) where

import Control.Monad.IO.Class (liftIO)
import Crypto.Sign.Ed25519
import Data.ByteString.Base64.URL (decodeBase64)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding
import Data.UUID (UUID, fromText)
import Network.Socket (SockAddr)
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth
import System.IO
import qualified Data.ByteString
import qualified Servant.Client as SC

import IAM.Authentication
import IAM.Authorization
import IAM.Client
import IAM.Policy (Effect(Allow, Deny))
import IAM.User (User(..))
import IAM.UserIdentifier
import IAM.UserPublicKey


data Auth
  = Unauthenticated !SockAddr
  | Authenticated !SockAddr !AuthHeaders !User


data AuthHeaders = AuthHeaders
  { unAuthHeadersKey :: !PublicKey
  , unAuthHeadersHost :: !Text
  , unAuthHeadersUser :: !Text
  , unAuthHeadersToken :: !Text
  , unAuthHeadersSignature :: !Signature
  , unAuthHeadersRequestID :: !UUID
  }


type instance AuthServerData (AuthProtect "signature-auth") = Auth


authContext :: Text -> SC.ClientEnv -> Context (AuthHandler Request Auth ': '[])
authContext host clientEnv = authHandler host clientEnv :. EmptyContext



authHandler :: Text -> SC.ClientEnv -> AuthHandler Request Auth
authHandler host clientEnv = mkAuthHandler handler where
  authenticate :: Request -> Handler (Either Auth (SockAddr, AuthHeaders))
  authenticate req = do
    let addr = remoteHost req
    case authRequestHeaders req of
      Nothing -> return $ Left $ Unauthenticated addr
      Just headers ->
        let signed = stringToSign method host' path query requestId (Just token)
            host' = encodeUtf8 $ unAuthHeadersHost headers
            path = rawPathInfo req
            query = rawQueryString req
            method = requestMethod req
            token = unAuthHeadersToken headers
            requestId = unAuthHeadersRequestID headers
            signature = unAuthHeadersSignature headers
            validHost = host == decodeUtf8 host'
            validSignature = dverify (unAuthHeadersKey headers) signed signature
            errorMessage = if not validHost
              then "Invalid host"
              else "Invalid signature"
         in if not (validHost && validSignature)
          then throwError $ err403 { errBody = errorMessage }
          else return $ Right (addr, headers)
  authorize :: Request -> SockAddr -> AuthHeaders -> User -> Handler Auth
  authorize req addr headers user = do
    let uident = userIdentifierFromText $ unAuthHeadersUser headers
        auth = AuthorizationRequest
          { authorizationRequestUser = uident
          , authorizationRequestHost = unAuthHeadersHost headers
          , authorizationRequestAction = actionFromMethod $ requestMethod req
          , authorizationRequestResource = decodeUtf8 $ rawPathInfo req
          , authorizationRequestToken = Just $ unAuthHeadersToken headers
          }
    result <- liftIO $ SC.runClientM (authorizeClient auth) clientEnv
    case result of
      Right (AuthorizationResponse Allow) ->
        return $ Authenticated addr headers user
      Right (AuthorizationResponse Deny) ->
        throwError err403
      Left e -> do
        liftIO $ putStrLn "Authorization failed"
        liftIO $ print e >> hFlush stdout
        throwError err500
  handler :: Request -> Handler Auth
  handler req = do
    result <- authenticate req
    case result of
      Left auth -> return auth
      Right (addr, headers) -> do
        let uident = userIdentifierFromText $ unAuthHeadersUser headers
            userClient = mkUserClient uident
        userResult <- liftIO $ SC.runClientM (getUser userClient) clientEnv
        case userResult of
          Left e -> do
            liftIO $ putStrLn "Get user failed"
            liftIO $ print e >> hFlush stdout
            throwError err500
          Right user ->
            if validPublicKey user (unAuthHeadersKey headers)
              then authorize req addr headers user
              else return $ Unauthenticated addr
  validPublicKey :: User -> PublicKey -> Bool
  validPublicKey user key =
    case find ((== key) . userPublicKey) (userPublicKeys user) of
      Just _ -> True
      Nothing -> False


authRequestHeaders :: Request -> Maybe AuthHeaders
authRequestHeaders req = do
  authorization <- lookup "Authorization" $ requestHeaders req
  host <- lookup "Host" $ requestHeaders req
  user <- lookup "X-MTaylor-IO-User-Id" $ requestHeaders req
  token <- lookup "X-MTaylor-IO-Session-Token" $ requestHeaders req
  key <- lookup "X-MTaylor-IO-Public-Key" $ requestHeaders req
  requestID <- lookup "X-MTaylor-IO-Request-ID" $ requestHeaders req
  requestID' <- fromText $ decodeUtf8 requestID
  case Data.ByteString.splitAt 10 authorization of
    ("Signature ", signature) ->
      case (decodeBase64 key, decodeBase64 signature) of
        (Right key', Right signature') ->
          return $ AuthHeaders
            { unAuthHeadersKey = PublicKey key'
            , unAuthHeadersHost = decodeUtf8 host
            , unAuthHeadersUser = decodeUtf8 user
            , unAuthHeadersToken = decodeUtf8 token
            , unAuthHeadersSignature = Signature signature'
            , unAuthHeadersRequestID = requestID'
            }
        (_, _) -> Nothing
    (_, _) -> Nothing
