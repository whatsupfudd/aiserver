{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Service.Flux where

import Data.Functor ((<&>))
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu

import GHC.Generics (Generic)

import Hasql.Pool (Pool)

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Client.TLS as Hct
import qualified Network.HTTP.Types.Status as Hs

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ak
import qualified Data.Aeson.Key as Ak

import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr

import qualified Assets.Storage as S3
import qualified Assets.Types as S3

import qualified DB.Opers as Db
import qualified Service.DbOps as Sdb


import Service.Types
import Control.Concurrent (threadDelay)

spanInvocation :: ServiceContext -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String [ServiceResult])
spanInvocation srvCtxt request tranz = do
  if srvCtxt.functionD.label `L.elem` ["text_to_image", "edit_image"] then do
    handleRequestFor srvCtxt request
  else
    let
      errMsg = "@[spanInvocation] unknown function: " <> unpack srvCtxt.functionD.label
    in do
    putStrLn errMsg
    pure . Left $ errMsg


handleRequestFor :: ServiceContext -> Rq.InvokeRequest ->  IO (Either String [ServiceResult])
handleRequestFor srvCtxt request =
  case srvCtxt.functionD.label of
    "text_to_image" ->
      case extractInvokeItems request of
        Left err ->
          pure $ Left err
        Right (params, content) -> do
          manager <- Hc.newManager Hct.tlsManagerSettings
          sendRequest (srvCtxt, manager) params content >>= \case
            Left err ->
              pure $ Left err
            Right requestID ->
              waitReply (srvCtxt, manager) 0 requestID >>= \case
                Left err ->
                  pure $ Left err
                Right assetURL ->
                  fetchAsset (srvCtxt, manager) request assetURL >>= \case
                    Left err ->
                      pure $ Left err
                    Right newAsset ->
                      pure $ Right [ AssetSR newAsset ]
    _ ->
      pure $ Left "Not implemented."


newtype FluxReply1 = FluxReply1 {
    id :: String
  }
  deriving (Show, Generic, Ae.FromJSON)


data FluxReply2 = FluxReply2 {
      status :: String
    , result :: Maybe FluxResult2
  }
  deriving (Show, Generic, Ae.FromJSON)

newtype FluxResult2 = FluxResult2 {
    sample :: String
  }
  deriving (Show, Generic, Ae.FromJSON)


sendRequest :: (ServiceContext, Hc.Manager) -> Mp.Map Text Text -> Text -> IO (Either String String)
sendRequest (srvCtxt, manager) params content = do
  baseRequest <- Hc.parseRequest "POST https://api.us1.bfl.ai/v1/flux-dev"

  let request = baseRequest {
      Hc.requestHeaders = [("Content-Type", "application/json"), ("X-Key", srvCtxt.apiKey)],
      Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode $ Ae.object ["prompt" Ae..= content ]
  }
  response <- Hc.httpLbs request manager
  case Hs.statusCode response.responseStatus of
      200 -> case Ae.eitherDecode response.responseBody :: Either String FluxReply1 of
          Right reply -> do
            putStrLn $ "@[sendRequest] flux id: " <> reply.id
            pure $ Right reply.id
          Left err -> do
            putStrLn $ "@[sendRequest] json decoding err: " <> err
            pure $ Left err
      _ -> do
          putStrLn $ "@[sendRequest] generation request err: " <> show (Hc.responseStatus response)
          pure . Left $ "@[sendRequest] http err: " <> show (Hc.responseStatus response)


waitReply :: (ServiceContext, Hc.Manager) -> Int32 -> String -> IO (Either String String)
waitReply (srvCtxt, manager) counter requestID = do
  let statusRequest = Hc.parseRequest_ $ "GET https://api.us1.bfl.ai/v1/get_result?id=" <> requestID
  let request = statusRequest {
      Hc.requestHeaders = [("X-Key", srvCtxt.apiKey)]
  }

  response <- Hc.httpLbs request manager
  putStrLn $ "@[waitReply] response: " <> show (Hc.responseBody response)
  case Ae.eitherDecode (Hc.responseBody response) :: Either String FluxReply2 of
      Right reply -> case reply.status of
        "Ready" -> case reply.result of
          Just result ->
            pure $ Right result.sample
          Nothing -> do
            putStrLn "@[waitReply] no result in reply although status is Ready"
            threadDelay 1000000  -- Wait 1 second before checking again
            if counter < 30 then
              waitReply (srvCtxt, manager) (counter + 1) requestID
            else
              pure $ Left "@[waitReply] error, got Ready but no result."
        _ -> do
          threadDelay 1000000  -- Wait 1 second before checking again
          if counter < 30 then
            waitReply (srvCtxt, manager) (counter + 1) requestID
          else
            pure . Left $ "@[waitReply] non-desirable status: " <> reply.status <> ", req: " <> requestID
      Left err -> do
        putStrLn $ "@[waitReply] error decoding status response: " <> err
        pure . Left $ "@[waitReply] error decoding status response: " <> err


{-
  response <- Hc.httpLbs request manager
  case Hs.statusCode response.responseStatus of
      200 -> do
      _ -> pure . Left $ "@[fetchAsset] http err: " <> show (Hc.responseStatus response)
-}

fetchAsset :: (ServiceContext, Hc.Manager) -> Rq.InvokeRequest -> String -> IO (Either String S3.Asset)
fetchAsset (srvCtxt, manager) request imageURL = do
  request <- Hc.parseRequest imageURL
  newID <- Uu.nextRandom
  let
    newAsset = S3.prepareNewAsset newID "Flux" "image/png"
  rezA <- S3.insertNewAsset srvCtxt.dbPool srvCtxt.s3Conn manager request newAsset
  case rezA of
    Left err ->
      let
        errMsg = "@[fetchAsset] Error inserting asset: " <> err
      in do
      putStrLn errMsg
      pure $ Left errMsg
    Right updAsset -> do
      putStrLn $ "@[fetchAsset] Asset inserted: " <> show updAsset
      pure $ Right updAsset

