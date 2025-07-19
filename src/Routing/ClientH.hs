{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Routing.ClientH where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as Gm

import qualified Data.ByteString.Lazy as Lbs
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import Data.ByteString (ByteString)
import Data.Text (Text, unpack, pack)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Servant.API.Generic
import Servant.API (JSON, PlainText, ReqBody, Get, Post, Delete, OctetStream, (:>), Capture, NoContent, Header, Raw, addHeader, Headers)
import Servant.Auth.Server (Auth, JWT, BasicAuth, AuthResult (..))
import Servant.Server.Generic (AsServerT, genericServerT)

import Api.Types (ClientInfo, AIServerApp, ApiError (..), AppEnv (..), fakeClientInfo, Html (..), SessionItems (..))
import qualified DB.Opers as DbB
import qualified Assets.Storage as DbA
import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr
import qualified Routing.ClientR as Cr
import qualified Api.Session as Sess
import qualified Service.Opers as Ops


-- Anonymous Handlers:
publicHandlers :: ToServant Cr.PublicRoutes (AsServerT AIServerApp)
publicHandlers = genericServerT $ Cr.PublicRoutes {
    loginPR = loginHandler
    , helpPR = helpHandler
    -- , homePage = homePageHandler
  }


loginHandler :: Rq.LoginForm -> AIServerApp Rr.LoginResult
loginHandler form = do
  liftIO $ putStrLn $ "@[loginHandler] user:" <> unpack form.username <> ", pwd: "
      <> unpack form.secret
  -- TODO: check for u/p validity, if so return new Session, otherwise throw error:
  Sess.loginUser form

  {-
  fakeClient <- liftIO fakeClientInfo
  pure . Rr.AuthenticatedLR $ SessionItems {
      sessionCtxt = fakeClient
      , jwt = "fake-jwt"
    }

    -- Session.loginUser form
    --  LoginResult {
      context = (ClientInfo 1 <time>)
      , jwt = (decodeUtf8 . LBS.toStrict $ "<fake-jwt>")
    --

    let
      mbRez = Nothing
    case mbRez of
      Nothing -> throwError UnauthorizedAE
                    -- $ "Invalid login credentials for : " <> DT.unpack form.username
      Just aResult -> pure aResult
  -}


helpHandler :: Maybe Text -> AIServerApp Html
helpHandler mbFilter = do
  liftIO $ putStrLn $ "@[helpHandler] filter: " <> show mbFilter
  pure . Html $ "<html><head><title>HELP!</title></head><body>HELP</body></html>"


{- Old stuff:
getRequestHandler :: AuthResult ClientInfo -> String -> AIServerApp Rr.ReplyClientRequest
getRequestHandler authResult pageUrl = do
  authCtxt <- fromAuthResult authResult
  -- Getting here means the authResult is valid.
  pure $ Rr.ReplyClientRequest "OK"

-}



fromAuthResult :: AuthResult a -> AIServerApp a
fromAuthResult (Authenticated uid) = return uid
fromAuthResult x =
  throwError UnauthorizedAE
    -- . DT.pack . show $ x


privateHandlers :: AuthResult ClientInfo -> ToServant Cr.PrivateRoutes (AsServerT AIServerApp)
privateHandlers authResult = genericServerT $ Cr.PrivateRoutes {
    client = clientHandlers authResult
  , repeat = repeatHandlers authResult
  , invoke = invokeHandlers authResult
  , asset = assetHandlers authResult
  , retrieve = retrieveHandlers authResult
  , storage = storageHandlers authResult
  , admin = adminHandlers authResult
  }


--- Client:

clientHandlers :: AuthResult ClientInfo -> ToServant Cr.ClientRoutes (AsServerT AIServerApp)
clientHandlers authResult = genericServerT $ Cr.ClientRoutes {
  getClient = getClientHandler authResult
  , updatePassword = updatePasswordHandler authResult
  , updatePreferences = updatePreferencesHandler authResult
  , listServices = listServicesHandler authResult
  }


getClientHandler :: AuthResult ClientInfo -> UUID -> AIServerApp ClientInfo
getClientHandler authResult clientId =
  liftIO fakeClientInfo


updatePasswordHandler authResult updatePassword = do
  liftIO $ putStrLn $ "@[updatePasswordHandler] updatePassword: " <> show updatePassword
  pure $ Rr.ReplyClientRequest "OK"


updatePreferencesHandler :: AuthResult ClientInfo -> Rq.UpdatePreferences -> AIServerApp Rr.ReplyClientRequest
updatePreferencesHandler authResult updatePreferences = do
  liftIO $ putStrLn $ "@[updatePreferencesHandler] updatePreferences: " <> show updatePreferences
  pure $ Rr.ReplyClientRequest "OK"


listServicesHandler :: AuthResult ClientInfo -> Maybe Text -> AIServerApp [Rr.ClientServiceDescriptor]
listServicesHandler authResult mbFilter = do
  liftIO $ putStrLn $ "@[listServicesHandler] filter: " <> show mbFilter
  case Uu.fromString "123e4567-e89b-12d3-a456-426614174000" of
    Nothing -> throwError $ InternalErrorAE "@[listServicesHandler] Failed to parse UUID"
    Just id ->
      let
        fakeServices = [
            Rr.ClientServiceDescriptor {
              id = id
              , name = "Service 1"
              , descriptions = "Description 1"
            }
          ]
      in
      pure fakeServices


--- Repeat previous requests:

repeatHandlers :: AuthResult ClientInfo -> ToServant Cr.RepeatRoutes (AsServerT AIServerApp)
repeatHandlers authResult = genericServerT $ Cr.RepeatRoutes {
    repeatOp = repeatOpHandler authResult
  }

repeatOpHandler :: AuthResult ClientInfo -> Rq.RepeatRequest -> AIServerApp Rr.ReplyClientRequest
repeatOpHandler authResult repeatRequest = do
  liftIO $ putStrLn $ "@[repeatOpHandler] repeatRequest: " <> show repeatRequest 
  pure $ Rr.ReplyClientRequest "OK"


--- Invoke:
{-
  getResource :: route :- "resource" :> ReqBody '[JSON] Rq.ResourceRequest :> Post '[JSON] Rr.InvokeResponse
  , fetchResult :: route :- "fetch" :> QueryParam"tid" UUID :> QueryParam' '[Optional] "mode" Text :> Get '[JSON] Rr.InvokeResponse
  , invokeService :: route :- ReqBody '[JSON] Rq.InvokeRequest :> Post '[JSON] Rr.InvokeResponse
-}
invokeHandlers :: AuthResult ClientInfo -> ToServant Cr.InvokeRoutes (AsServerT AIServerApp)
invokeHandlers authResult = genericServerT $ Cr.InvokeRoutes {
    getResource = getResourceHandler authResult
  , getResponse = getResponseHandler authResult
  , invokeService = invokeServiceHandler authResult
  }


newtype TestPayload = TestPayload {
    aField :: Text
  }
  deriving (Show, Generic, Ae.ToJSON)
instance Ae.FromJSON TestPayload where
  parseJSON = Ae.withObject "TestPayload" $ \v ->
    TestPayload <$> v Ae..: "aField"

data TestResult = TestResult {
  id :: UUID
  , name :: Text
  , description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass Ae.ToJSON

data TestError = TestError {
  code :: Text
  , message :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass Ae.ToJSON


invokeServiceHandler :: AuthResult ClientInfo -> Rq.InvokeRequest -> AIServerApp Rr.InvokeResponse
invokeServiceHandler (Authenticated clientInfo) request = do
  appEnv <- Gm.ask
  eiValidClient <- liftIO $ DbB.checkValidClient appEnv.pgPool_Ctxt clientInfo
  case eiValidClient of
    Left err ->
      let
        errMsg = "@[invokeServiceHandler] checkValidClient err: " <> err
      in do
        liftIO $ putStrLn errMsg
        throwError . InternalErrorAE $ pack errMsg
    Right checkResult ->
      if checkResult then do
        liftIO $ putStrLn $ "@[invokeServiceHandler] valid client: " <> show clientInfo
        rezInv <- liftIO $ Ops.processInvocation appEnv clientInfo request
        case rezInv of
          Left err -> do
            liftIO $ putStrLn $ "@[invokeServiceHandler] processInvocation err: " <> err
            throwError . InternalErrorAE $ pack err
          Right rez -> do
            -- liftIO $ putStrLn $ "@[invokeServiceHandler] processInvocation rez: " <> show rez
            pure rez
      else
        let
          errMsg = "@[invokeServiceHandler] invalid client: " <> show clientInfo
        in do
        liftIO $ putStrLn errMsg
        throwError . InternalErrorAE $ pack errMsg
      {-
      liftIO $ putStrLn $ "@[invokeServiceHandler] request: " <> show request
      case Uu.toString request.service of  
        "123e4567-e89b-12d3-a456-426614174000" ->
          case Ae.fromJSON request.payload :: Ae.Result TestPayload of
            Ae.Success payload -> do
              liftIO $ putStrLn $ "@[invokeServiceHandler] payload: " <> show payload
              let
                returnValue = TestResult {
                  id = Uu.nil
                  , name = "Test Result"
                  , description = "Test Result Description"
                }
              liftIO $ Rr.fakeServiceResponse (Ae.toJSON payload) "OK"
            Ae.Error err -> do
              liftIO $ putStrLn $ "@[invokeServiceHandler]can't decode: " <> show request.payload <> " error: " <> err
              liftIO $ Rr.fakeServiceResponse (Ae.toJSON $ TestError "123" "@[invokeServiceHandler] Can't decode payload") "ERROR"
        _ ->
          liftIO $ Rr.fakeServiceResponse (Ae.toJSON $ "@[invokeServiceHandler] unknown svcId: " <> show request.service) "ERROR"
      -}

invokeServiceHandler authResult request =
  let
    errMsg = "@[invokeServiceHandler] invalid authResult: " <> show authResult
  in do
    liftIO $ putStrLn errMsg
    throwError UnauthorizedAE


getResourceHandler :: AuthResult ClientInfo -> Rq.ResourceRequest -> AIServerApp Rr.InvokeResponse
getResourceHandler authResult resourceRequest = do
  liftIO $ putStrLn $ "@[getResourceHandler] resourceRequest: " <> show resourceRequest
  liftIO $ Rr.fakeServiceResponse (Ae.toJSON $ TestError "123" "@[getResourceHandler] Can't decode payload") "ERROR"


getResponseHandler :: AuthResult ClientInfo -> Maybe UUID -> Maybe Text -> AIServerApp Rr.InvokeResponse
getResponseHandler authResult mbTid mbMode = do
  case mbTid of
    Nothing -> do
      liftIO $ putStrLn $ "@[getResponseHandler] tid is missing"
      throwError . InternalErrorAE $ pack "@[getResultHandler] tid is missing"
    Just tid -> do
      appEnv <- Gm.ask
      rezA <- liftIO $ DbB.getResponse appEnv.pgPool_Ctxt tid
      case rezA of
        Left err -> do
          liftIO $ putStrLn $ "@[getResponseHandler] getResponse err: " <> err
          throwError . InternalErrorAE $ pack err
        Right aResponse -> do
          liftIO $ putStrLn $ "@[getResponseHandler] tid: " <> show tid <> " mode: " <> show mbMode
          pure $ Rr.InvokeResponse {
            requestID = 0
            , requestEId = tid
            , contextID = 0
            , contextEId = tid
            , status = "OK"
            , result = Ae.toJSON aResponse
          }


-- Assets:
assetHandlers :: AuthResult ClientInfo -> ToServant Cr.AssetRoutes (AsServerT AIServerApp)
assetHandlers authResult = genericServerT $ Cr.AssetRoutes {
    getAsset = getAssetHandler authResult
  }

getAssetHandler :: AuthResult ClientInfo -> UUID -> Maybe Text -> AIServerApp (Headers '[Header "Content-Type" Text] ByteString)
getAssetHandler authResult assetId mbP = do
  appEnv <- Gm.ask
  case appEnv.s3Storage_Ctxt of
    Nothing -> do
      liftIO $ putStrLn $ "@[getAssetHandler] s3Storage_Ctxt is not set"
      throwError . InternalErrorAE $ pack "@[getAssetHandler] s3Storage_Ctxt is not set"
    Just s3Conn -> do
      rezA <- liftIO $ DbA.getAsset appEnv.pgPool_Ctxt s3Conn assetId
      case rezA of
        Left err -> do
          liftIO $ putStrLn $ "@[getAssetHandler] getAsset err: " <> err
          throwError . InternalErrorAE $ pack err
        Right (contentType, assetData) -> do
          liftIO $ putStrLn $ "@[getAssetHandler] assetId: " <> show assetId <> " p: " <> show mbP
          pure $ addHeader contentType (Lbs.toStrict assetData)


-- Retrieve:
retrieveHandlers :: AuthResult ClientInfo -> ToServant Cr.RetrieveRoutes (AsServerT AIServerApp)
retrieveHandlers authResult = genericServerT $ Cr.RetrieveRoutes {
    retrieve = retrieveHandler authResult
  }

retrieveHandler :: AuthResult ClientInfo -> UUID -> AIServerApp Rr.InvokeResponse
retrieveHandler authResult requestId = do
  liftIO $ putStrLn $ "@[retrieveHandler] requestId: " <> show requestId 
  liftIO $ Rr.fakeServiceResponse (Ae.toJSON $ TestError "123" "@[retriveHandler] Can't decode payload") "ERROR"


-- Storage:
storageHandlers :: AuthResult ClientInfo -> ToServant Cr.StorageRoutes (AsServerT AIServerApp)
storageHandlers authResult = genericServerT $ Cr.StorageRoutes {
    storagePut = storagePutHandler authResult
  , storageGet = storageGetHandler authResult
  , storageDel = storageDelHandler authResult
  }

storagePutHandler :: AuthResult ClientInfo -> Rq.StoragePut -> AIServerApp Rr.ReplyClientRequest
storagePutHandler authResult storagePut = do
  liftIO $ putStrLn $ "@[storagePutHandler] storagePut: " <> show storagePut 
  pure $ Rr.ReplyClientRequest "OK"

storageGetHandler :: AuthResult ClientInfo -> UUID -> AIServerApp ByteString
storageGetHandler authResult itemId = do
  liftIO $ putStrLn $ "@[storageGetHandler] itemId: " <> show itemId 
  pure "OK"

storageDelHandler :: AuthResult ClientInfo -> UUID -> AIServerApp Rr.ReplyClientRequest
storageDelHandler authResult itemId = do
  liftIO $ putStrLn $ "@[storageDelHandler] itemId: " <> show itemId 
  pure $ Rr.ReplyClientRequest "OK"


--- Administration:

adminHandlers :: AuthResult ClientInfo -> ToServant Cr.AdminRoutes (AsServerT AIServerApp)
adminHandlers authResult = genericServerT $ Cr.AdminRoutes {
    svcCreate = svcCreateHandler authResult
  , svcGet = svcGetHandler authResult
  , quotaUpdate = quotaUpdateHandler authResult
  , newClient = createClientHandler authResult
  }

svcCreateHandler :: AuthResult ClientInfo -> Rq.ServiceDescriptor -> AIServerApp Rr.ReplyClientRequest
svcCreateHandler authResult serviceDescriptor = do
  liftIO $ putStrLn $ "@[svcCreateHandler] serviceDescriptor: " <> show serviceDescriptor 
  pure $ Rr.ReplyClientRequest "OK"

svcGetHandler :: AuthResult ClientInfo -> UUID -> AIServerApp Rr.ReplyClientRequest
svcGetHandler authResult svcId = do
  liftIO $ putStrLn $ "@[svcGetHandler] svcId: " <> show svcId 
  pure $ Rr.ReplyClientRequest "OK"

quotaUpdateHandler :: AuthResult ClientInfo -> UUID -> Rq.QuotaUpdate -> AIServerApp Rr.ReplyClientRequest
quotaUpdateHandler authResult clientId quotaUpdate = do
  liftIO $ putStrLn $ "@[quotaUpdateHandler] clientId: " <> show clientId 
            <> ", quotaUpdate: " <> show quotaUpdate 
  pure $ Rr.ReplyClientRequest "OK"

createClientHandler :: AuthResult ClientInfo -> Rq.ClientCreate -> AIServerApp ClientInfo
createClientHandler authResult clientCreate = do
  liftIO $ putStrLn $ "@[createClientHandler] clientCreate: " <> show clientCreate
  liftIO fakeClientInfo

