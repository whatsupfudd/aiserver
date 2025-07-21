module Service.ElevenLabs where

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

import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr

import qualified Assets.Storage as S3

import qualified DB.Opers as Db
import qualified Service.DbOps as Sdb


import Service.Types
import Control.Concurrent (threadDelay)

spanInvocation :: ServiceContext -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String [ServiceResult])
spanInvocation srvCtxt request tranz = do
  if srvCtxt.functionD.label `L.elem` ["text_to_speech", "list_voices", "list_models"] then do
    handleRequestFor srvCtxt request
  else
    let
      errMsg = "@[spanInvocation] unknown function: " <> unpack srvCtxt.functionD.label
    in do
    putStrLn errMsg
    pure . Left $ errMsg


{-
text to speech:
curl -X POST "https://api.elevenlabs.io/v1/text-to-speech/JBFqnCBsd6RMkjVDRZzb?output_format=mp3_44100_128" \
     -H "xi-api-key: xi-api-key" \
     -H "Content-Type: application/json" \
     -d '{
    "text": "The first move is what sets everything in motion.",
    "model_id": "eleven_multilingual_v2" }'

list_voices:
curl https://api.elevenlabs.io/v2/voices \
     -H "xi-api-key: xi-api-key"

data InvokeRequest = InvokeRequest { 
    function :: UUID
  , context :: Maybe UUID
  , parameters :: Value
  , content :: Value
  , files :: [UUID]
  , references :: [UUID]
  } deriving (Show, Eq, Generic, FromJSON)

-}


handleRequestFor :: ServiceContext -> Rq.InvokeRequest ->  IO (Either String [ServiceResult])
handleRequestFor srvCtxt request =
  case srvCtxt.functionD.label of
    "text_to_speech" -> do
      case Ae.fromJSON request.parameters :: Ae.Result ParamContainer of
        Ae.Error err -> do
          putStrLn $ "Error parsing parameters: " <> err
          pure $ Left "Error parsing parameters"
        Ae.Success paramContainer -> do
          putStrLn $ "Parameters: " <> show paramContainer
          let
            voiceID = case L.find (\p -> p.name == "voice") paramContainer.params of
              Just (ParameterValue _ voiceID) -> unpack voiceID
              Nothing -> "JBFqnCBsd6RMkjVDRZzb"
          case Ae.fromJSON request.content :: Ae.Result Text of
            Ae.Error err -> do
              putStrLn $ "Error parsing content: " <> err
              pure $ Left "Error parsing content"
            Ae.Success line -> do
              putStrLn $ "Content: " <> show line
              let requestObject = Ae.object [
                    "text" Ae..= line
                    , "model_id" Ae..= ("eleven_multilingual_v2" :: Text)
                    ]
                  modelID = voiceID
                  outputFormat = "mp3_44100_128"
              manager <- Hc.newManager Hct.tlsManagerSettings
              initialRequest <- Hc.parseRequest $ "https://api.elevenlabs.io/v1/text-to-speech/" <> modelID <> "?output_format=" <> outputFormat
              let request = initialRequest {
                  Hc.method = "POST"
                  , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode requestObject
                  , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey), ("Content-Type", "application/json")]
                  }
              -- putStrLn $ "Request: " <> show request
              assetEid <- Uu.nextRandom
              let
                newAsset = S3.prepareNewAsset assetEid "ElevenLabs" "audio/mp3"
              rezA <- S3.insertNewAsset srvCtxt.dbPool srvCtxt.s3Conn manager request newAsset 
              case rezA of
                Left err -> do
                  putStrLn $ "Error inserting asset: " <> err
                  pure $ Left "Error inserting asset"
                Right updAsset -> do
                  putStrLn $ "Asset inserted: " <> show updAsset
                  pure . Right $ [ AssetSR updAsset ]
    "list_voices" -> do
      manager <- Hc.newManager Hct.tlsManagerSettings
      initialRequest <- Hc.parseRequest "https://api.elevenlabs.io/v2/voices"
      let request = initialRequest {
          Hc.method = "GET"
          , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey)]
          }
      response <- Hc.httpLbs request manager
      case Hs.statusCode response.responseStatus of
        200 ->
          pure . Right $ [ TextReplySR JsonRK . T.decodeUtf8 $ Lbs.toStrict response.responseBody ]
        _ ->
          pure . Left $ "@[handleRequestFor] list_voices err: " <> (show . Hs.statusCode) response.responseStatus
    "list_models" -> do
      manager <- Hc.newManager Hct.tlsManagerSettings
      initialRequest <- Hc.parseRequest "https://api.elevenlabs.io/v1/models"
      let request = initialRequest {
          Hc.method = "GET"
          , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey)]
          }
      response <- Hc.httpLbs request manager
      case Hs.statusCode response.responseStatus of
        200 ->
          pure . Right $ [ TextReplySR JsonRK . T.decodeUtf8 $ Lbs.toStrict response.responseBody ]
        _ ->
          pure . Left $ "@[handleRequestFor] list_voices err: " <> (show . Hs.statusCode) response.responseStatus
    _ ->
      pure . Left $ "@[handleRequestFor] ElevenLabs: function not supported: " <> unpack srvCtxt.functionD.label

