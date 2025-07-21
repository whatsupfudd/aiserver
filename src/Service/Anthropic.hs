{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Service.Anthropic where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (fromMaybe)
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
import Text.Read (readMaybe)

spanInvocation :: ServiceContext -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String [ServiceResult])
spanInvocation srvCtxt request tranz = do
  if srvCtxt.functionD.label `L.elem` ["message", "list_models", "model_info"] then do
    handleRequestFor srvCtxt request
  else
    let
      errMsg = "@[spanInvocation] unknown function: " <> unpack srvCtxt.functionD.label
    in do
    putStrLn errMsg
    pure . Left $ errMsg


{-
Message api:
curl https://api.anthropic.com/v1/messages \
     --header "x-api-key: $ANTHROPIC_API_KEY" \
     --header "anthropic-version: 2023-06-01" \
     --header "content-type: application/json" \
     --data \
'{
    "model": "claude-sonnet-4-20250514",
    "max_tokens": 1024,
    "messages": [
        {"role": "user", "content": "Hello, world"}
    ]
}'
-}

data RequestC = RequestC {
  model :: Text,
  maxTokens :: Int,
  messages :: [MessageC]
  }
  deriving (Show, Generic)

instance Ae.ToJSON RequestC where
  toJSON aReq =
    Ae.object [
      "model" Ae..= aReq.model,
      "max_tokens" Ae..= aReq.maxTokens,
      "messages" Ae..= aReq.messages
    ]

data MessageC = MessageC {
  role :: Text,
  content :: Text
  }
  deriving (Show, Generic, Ae.ToJSON)

data ReplyMsgC = ReplyMsgC {
    content :: [ContentC]
    , id :: Text
    , model :: Text
    , role :: Text
    , stop_sequences :: Maybe [Text]
    , stop_reason :: Text
    , type_ :: Text
    , usage :: UsageC
  }
  deriving (Show, Generic)

instance Ae.FromJSON ReplyMsgC where
  parseJSON = Ae.withObject "ReplyMsgC" $ \o -> do
    ReplyMsgC <$> o Ae..: "content"
    <*> o Ae..: "id"
    <*> o Ae..: "model"
    <*> o Ae..: "role"
    <*> o Ae..:? "stop_sequences"
    <*> o Ae..: "stop_reason"
    <*> o Ae..: "type"
    <*> o Ae..: "usage"

data ContentC = ContentC {
    text :: Text
    , type_ :: Text
  }
  deriving (Show, Generic)

instance Ae.FromJSON ContentC where
  parseJSON = Ae.withObject "ContentC" $ \o -> do
    ContentC <$> o Ae..: "text"
    <*> o Ae..: "type"

data UsageC = UsageC {
    input_tokens :: Int
    , output_tokens :: Int
  }
  deriving (Show, Generic, Ae.FromJSON)

handleRequestFor :: ServiceContext -> Rq.InvokeRequest ->  IO (Either String [ServiceResult])
handleRequestFor srvCtxt request =
  case srvCtxt.functionD.label of
    "message" ->
      case extractInvokeItems request of
        Left err ->
          pure $ Left err
        Right (params, content) -> do
          manager <- Hc.newManager Hct.tlsManagerSettings
          let
            genModel = fromMaybe "claude-3-haiku-20240307" (Mp.lookup "model" params)
            maxTokens = fromMaybe 1024 $ Mp.lookup "max_tokens" params >>= readMaybe . unpack
          let
            baseRequest = Hc.parseRequest_ "POST https://api.anthropic.com/v1/messages"
            messages = [ MessageC { role = "user", content = content } ]
            chatRequest = RequestC { model = genModel, maxTokens = 1024, messages = messages  }
            request = baseRequest {
              Hc.requestHeaders = [("x-api-key", srvCtxt.apiKey)
                          , ("anthropic-version", "2023-06-01"), ("content-type", "application/json")]
              , Hc.requestBody = Hc.RequestBodyLBS (Ae.encode chatRequest)
            }
          rezA <- Hc.httpLbs request manager
          case Hs.statusCode rezA.responseStatus of
            200 -> do
              case Ae.eitherDecode rezA.responseBody :: Either String ReplyMsgC of
                Left err ->
                  pure $ Left $ "@[handleRequestFor] json decoding err: " <> err
                Right response ->
                  let
                    content = if L.null response.content then
                        "<void>"
                      else
                        let
                          lh = L.head response.content
                        in
                        lh.text
                  in do
                  putStrLn $ "@[handleRequestFor] response: " <> show response
                  pure $ Right [ TextReplySR PlainTextRK content ]
            _ -> pure $ Left $ "@[handleRequestFor] http err: " <> show (Hs.statusCode rezA.responseStatus) <> "\n body: " <> show rezA.responseBody
    _ ->
      pure $ Left "Not implemented."
