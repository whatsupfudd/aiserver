{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Service.ElevenLabs where

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import Data.Text (Text, unpack, pack)
import qualified Data.List as L

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

import qualified DB.Opers as Db
import qualified Service.DbOps as Sdb


import Service.Types

spanInvocation :: ServiceContext -> Pool -> Text -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String Int32)
spanInvocation srvCtxt dbPool fctName request tranz = do
  case fctName of
    "text_to_speech" -> do
      putStrLn "@[spanInvocation] text_to_speech."
      handleRequestFor srvCtxt fctName request
      pure . Right $ 1
    "list_voices" -> do
      putStrLn "@[spanInvocation] list_voices."
      handleRequestFor srvCtxt fctName request
      pure . Right $ 1
    "list_models" -> do
      putStrLn "@[spanInvocation] list_models."
      handleRequestFor srvCtxt fctName request
      pure . Right $ 1
    _ -> do
      putStrLn $ "@[spanInvocation] unknown function: " <> unpack fctName
      pure . Left $ "unknown function: " <> unpack fctName
  
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

data ParameterValue = ParameterValue {
    name :: Text
  , value :: Text
  } deriving (Show, Eq, Generic)


instance Ae.FromJSON ParameterValue where
  parseJSON (Ae.Object o) =
    let
      (label, value) = head $ Ak.toList o
      ident = case value of
        Ae.String s -> s
        Ae.Null -> ""
        _ -> "[ParameterValue.parseJSON] Invalid fct value: " <> (pack . show) value
    in
      pure (ParameterValue (pack . Ak.toString $ label) ident)


newtype ParamContainer = ParamContainer {
    params :: [ ParameterValue ]
  } deriving (Show, Eq, Generic)


instance Ae.FromJSON ParamContainer where
  parseJSON (Ae.Object o) =
    let
      paramList = [ ParameterValue (pack $ Ak.toString k) v | (k, Ae.String v) <- Ak.toList o ]
    in pure $ ParamContainer paramList
  parseJSON _ = fail "ParamContainer: Expected an object"

handleRequestFor :: ServiceContext -> Text -> Rq.InvokeRequest ->  IO (Either Text ())
handleRequestFor srvCtxt aFunction request =
  case aFunction of
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
            outputFile = case L.find (\p -> p.name == "output_file") paramContainer.params of
              Just (ParameterValue _ outputFile) -> unpack outputFile
              Nothing -> "/tmp/response.mp3"
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
              response <- Hc.httpLbs request manager
              putStrLn $ "The status code was: " <> show response.responseStatus.statusCode
              Lbs.writeFile outputFile response.responseBody
              pure $ Right ()
    "list_voices" -> do
      manager <- Hc.newManager Hct.tlsManagerSettings
      initialRequest <- Hc.parseRequest "https://api.elevenlabs.io/v2/voices"
      let request = initialRequest {
          Hc.method = "GET"
          , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey)]
          }
      response <- Hc.httpLbs request manager
      putStrLn $ "The status code was: " <> show response.responseStatus.statusCode
      Lbs.writeFile "/tmp/response.json" response.responseBody
      pure $ Right ()
    "list_models" -> do
      manager <- Hc.newManager Hct.tlsManagerSettings
      initialRequest <- Hc.parseRequest "https://api.elevenlabs.io/v1/models"
      let request = initialRequest {
          Hc.method = "GET"
          , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey)]
          }
      response <- Hc.httpLbs request manager
      putStrLn $ "The status code was: " <> show response.responseStatus.statusCode
      Lbs.writeFile "/tmp/response.json" response.responseBody
      pure $ Right ()
    _ ->
      pure $ Left "Function not supported"
