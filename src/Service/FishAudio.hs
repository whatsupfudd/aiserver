module Service.FishAudio where

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
  if srvCtxt.functionD.label == "text_to_speech" then do
    handleRequestFor srvCtxt request
  else
    let
      errMsg = "@[FishAudio.spanInvocation] unknown function: " <> unpack srvCtxt.functionD.label
    in do
    putStrLn errMsg
    pure . Left $ errMsg


{-
text to speech:
curl -X POST https://api.fish.audio/v1/tts \
  -H "Authorization: Bearer YOUR_API_KEY" \
  -H "Content-Type: application/json" \
  -H "model: s1" \
  -d '{"text": "Hello! Welcome to Fish Audio."}' \
  --output welcome.mp3list_voices:

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
              Nothing -> "41db41746b9c4bd18053c2bfc213b476"
          case Ae.fromJSON request.content :: Ae.Result Text of
            Ae.Error err -> do
              putStrLn $ "Error parsing content: " <> err
              pure $ Left "Error parsing content"
            Ae.Success line -> do
              putStrLn $ "Content: " <> show line
              let requestObject = Ae.object [
                    "text" Ae..= line
                    , "reference_id" Ae..= voiceID
                    , "format" Ae..= ("mp3" :: Text)
                    ]
                  modelID = voiceID
              manager <- Hc.newManager Hct.tlsManagerSettings
              initialRequest <- Hc.parseRequest "https://api.fish.audio/v1/tts"
              let request = initialRequest {
                  Hc.method = "POST"
                  , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode requestObject
                  , Hc.requestHeaders = [
                            ("Authorization", "Bearer " <> srvCtxt.apiKey)
                          , ("Content-Type", "application/json")
                          , ("model", "s1")
                      ]
                  , Hc.responseTimeout = Hc.responseTimeoutMicro (4 * 60 * 1000000)
                  }
              -- putStrLn $ "Request: " <> show request
              assetEid <- Uu.nextRandom
              let
                newAsset = S3.prepareNewAsset assetEid "FishAudio" "audio/mp3"
              rezA <- S3.insertNewAsset srvCtxt.dbPool srvCtxt.s3Conn manager request newAsset 
              case rezA of
                Left err -> 
                  let
                    errMsg = "@[handleRequestFor] Error processing request: " <> err
                  in do
                  -- putStrLn errMsg
                  pure $ Left errMsg
                Right updAsset -> do
                  -- putStrLn $ "Asset inserted: " <> show updAsset
                  pure . Right $ [ AssetSR updAsset ]
    _ ->
      pure . Left $ "@[handleRequestFor] ElevenLabs: function not supported: " <> unpack srvCtxt.functionD.label

