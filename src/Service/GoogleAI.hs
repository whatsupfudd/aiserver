{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Service.GoogleAI where

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Either as Ei
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
import Data.Maybe (isNothing, fromMaybe)

spanInvocation :: ServiceContext -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String [ServiceResult])
spanInvocation srvCtxt request tranz = do
  if srvCtxt.functionD.label `L.elem` ["message", "chain_of_thought", "text_to_image", "text_to_speech", "text_to_video", "edit_image"] then do
    manager <- Hc.newManager Hct.tlsManagerSettings
    handleRequestFor (srvCtxt, manager) request
  else
    let
      errMsg = "@[spanInvocation] unknown function: " <> unpack srvCtxt.functionD.label
    in do
    putStrLn errMsg
    pure . Left $ errMsg

{-
Text generation:
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{
    "contents": [
      {
        "parts": [
          {
            "text": "How does AI work?"
          }
        ]
      }
    ]
  }'

Chain of thought:
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{
    "contents": [
      {
        "parts": [
          {
            "text": "How does AI work?"
          }
        ]
      }
    ],
    "generationConfig": {
      "thinkingConfig": {
        "thinkingBudget": 0
      }
    }
  }'

System instructions:
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -d '{
    "system_instruction": {
      "parts": [
        {
          "text": "You are a cat. Your name is Neko."
        }
      ]
    },
    "contents": [
      {
        "parts": [
          {
            "text": "Hello there"
          }
        ]
      }
    ]
  }'

Multimodal inputs:
# Use a temporary file to hold the base64 encoded image data
TEMP_B64=$(mktemp)
trap 'rm -f "$TEMP_B64"' EXIT
base64 $B64FLAGS $IMG_PATH > "$TEMP_B64"

# Use a temporary file to hold the JSON payload
TEMP_JSON=$(mktemp)
trap 'rm -f "$TEMP_JSON"' EXIT

cat > "$TEMP_JSON" << EOF
{
  "contents": [
    {
      "parts": [
        {
          "text": "Tell me about this instrument"
        },
        {
          "inline_data": {
            "mime_type": "image/jpeg",
            "data": "$(cat "$TEMP_B64")"
          }
        }
      ]
    }
  ]
}
EOF

curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d "@$TEMP_JSON"

Conversation continuation:
curl https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{
    "contents": [
      {
        "role": "user",
        "parts": [
          {
            "text": "Hello"
          }
        ]
      },
      {
        "role": "model",
        "parts": [
          {
            "text": "Great to meet you. What would you like to know?"
          }
        ]
      },
      {
        "role": "user",
        "parts": [
          {
            "text": "I have two dogs in my house. How many paws are in my house?"
          }
        ]
      }
    ]
  }'

Text to Image:
curl -s -X POST 
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-preview-image-generation:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "contents": [{
      "parts": [
        {"text": "Hi, can you create a 3d rendered image of a pig with wings and a top hat flying over a happy futuristic scifi city with lots of greenery?"}
      ]
    }],
    "generationConfig":{"responseModalities":["TEXT","IMAGE"]}
  }' \
  | grep -o '"data": "[^"]*"' \
  | cut -d'"' -f4 \
  | base64 --decode > gemini-native-image.png


Image editing:
IMG_PATH=/path/to/your/image1.jpeg

if [[ "$(base64 --version 2>&1)" = *"FreeBSD"* ]]; then
  B64FLAGS="--input"
else
  B64FLAGS="-w0"
fi

IMG_BASE64=$(base64 "$B64FLAGS" "$IMG_PATH" 2>&1)

curl -X POST \
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-preview-image-generation:generateContent" \
    -H "x-goog-api-key: $GEMINI_API_KEY" \
    -H 'Content-Type: application/json' \
    -d "{
      \"contents\": [{
        \"parts\":[
            {\"text\": \"'Hi, This is a picture of me. Can you add a llama next to me\"},
            {
              \"inline_data\": {
                \"mime_type\":\"image/jpeg\",
                \"data\": \"$IMG_BASE64\"
              }
            }
        ]
      }],
      \"generationConfig\": {\"responseModalities\": [\"TEXT\", \"IMAGE\"]}
    }"  \
  | grep -o '"data": "[^"]*"' \
  | cut -d'"' -f4 \
  | base64 --decode > gemini-edited-image.png

Imagen-based image generation:
curl -X POST \
    "https://generativelanguage.googleapis.com/v1beta/models/imagen-4.0-generate-preview-06-06:predict" \
    -H "x-goog-api-key: $GEMINI_API_KEY" \
    -H "Content-Type: application/json" \
    -d '{
        "instances": [
          {
            "prompt": "Robot holding a red skateboard"
          }
        ],
        "parameters": {
          "sampleCount": 4
        }
      }'

Text to Video:
# Use curl to send a POST request to the predictLongRunning endpoint.
# The request body includes the prompt for video generation.
curl "${BASE_URL}/models/veo-3.0-generate-preview:predictLongRunning" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H "Content-Type: application/json" \
  -X "POST" \
  -d '{
    "instances": [{
        "prompt": "Panning wide shot of a purring kitten sleeping in the sunshine"
      }
    ],
    "parameters": {
      "aspectRatio": "16:9",
      "personGeneration": "allow_all",
    }
  }' | tee result.json | jq .name | sed 's/"//g' > op_name

# Obtain operation name to download video.
op_name=$(cat op_name)

# Check against status of operation.
while true; do
  is_done=$(curl -H "x-goog-api-key: $GEMINI_API_KEY" "${BASE_URL}/${op_name}" | tee op_check.json | jq .done)

  if [ "${is_done}" = "true" ]; then
    cat op_check.json
    echo "** Attach API_KEY to download video, or examine error message."
    break
  fi

  echo "** Video ${op_name} has not downloaded yet!  Check again after 5 seconds..."

  # Wait for 5 seoncds to check again.
  sleep 5

done

Text to Speech (mono-speaker):
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
        "contents": [{
          "parts":[{
            "text": "Say cheerfully: Have a wonderful day!"
          }]
        }],
        "generationConfig": {
          "responseModalities": ["AUDIO"],
          "speechConfig": {
            "voiceConfig": {
              "prebuiltVoiceConfig": {
                "voiceName": "Kore"
              }
            }
          }
        },
        "model": "gemini-2.5-flash-preview-tts",
    }' | jq -r '.candidates[0].content.parts[0].inlineData.data' | \
          base64 --decode >out.pcm
# You may need to install ffmpeg.
ffmpeg -f s16le -ar 24000 -ac 1 -i out.pcm out.wav

Text to Speech (multi-speaker):
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
  "contents": [{
    "parts":[{
      "text": "TTS the following conversation between Joe and Jane:
                Joe: Hows it going today Jane?
                Jane: Not too bad, how about you?"
    }]
  }],
  "generationConfig": {
    "responseModalities": ["AUDIO"],
    "speechConfig": {
      "multiSpeakerVoiceConfig": {
        "speakerVoiceConfigs": [{
            "speaker": "Joe",
            "voiceConfig": {
              "prebuiltVoiceConfig": {
                "voiceName": "Kore"
              }
            }
          }, {
            "speaker": "Jane",
            "voiceConfig": {
              "prebuiltVoiceConfig": {
                "voiceName": "Puck"
              }
            }
          }]
      }
    }
  },
  "model": "gemini-2.5-flash-preview-tts",
}' | jq -r '.candidates[0].content.parts[0].inlineData.data' | \
    base64 --decode > out.pcm
# You may need to install ffmpeg.
ffmpeg -f s16le -ar 24000 -ac 1 -i out.pcm out.wav

Voice list:
Zephyr -- Bright	Puck -- Upbeat	Charon -- Informative
Kore -- Firm	Fenrir -- Excitable	Leda -- Youthful
Orus -- Firm	Aoede -- Breezy	Callirrhoe -- Easy-going
Autonoe -- Bright	Enceladus -- Breathy	Iapetus -- Clear
Umbriel -- Easy-going	Algieba -- Smooth	Despina -- Smooth
Erinome -- Clear	Algenib -- Gravelly	Rasalgethi -- Informative
Laomedeia -- Upbeat	Achernar -- Soft	Alnilam -- Firm
Schedar -- Even	Gacrux -- Mature	Pulcherrima -- Forward
Achird -- Friendly	Zubenelgenubi -- Casual	Vindemiatrix -- Gentle
Sadachbia -- Lively	Sadaltager -- Knowledgeable	Sulafat -- Warm

Voice models:
gemini-2.5-flash-preview-tts
gemini-2.5-pro-preview-tts
-}

data GenerationReqC = GenerationReqC {
  model :: Text
  , contents :: [ContentC]
  , generationConfig :: Maybe GenerationConfigC
 } deriving (Show, Generic)

instance Ae.ToJSON GenerationReqC where
  toEncoding = Ae.genericToEncoding (Ae.defaultOptions {Ae.omitNothingFields = True})


newtype ContentC = ContentC {
  parts :: [PartC]
  } deriving (Show, Generic)
instance Ae.ToJSON ContentC where
  toEncoding = Ae.genericToEncoding (Ae.defaultOptions {Ae.sumEncoding = Ae.UntaggedValue })


data GenerationConfigC = GenerationConfigC {
    responseModalities :: [Text]
    , speechConfig :: Maybe SpeechConfigC
  } deriving (Show, Generic)

instance Ae.ToJSON GenerationConfigC where
  toEncoding = Ae.genericToEncoding (Ae.defaultOptions {Ae.omitNothingFields = True})


newtype SpeechConfigC = SpeechConfigC {
    voiceConfig :: VoiceConfigC
  } deriving (Show, Generic, Ae.ToJSON)

newtype VoiceConfigC = VoiceConfigC {
    prebuiltVoiceConfig :: PrebuiltVoiceConfigC
  } deriving (Show, Generic, Ae.ToJSON)

newtype PrebuiltVoiceConfigC = PrebuiltVoiceConfigC {
    voiceName :: Text
  } deriving (Show, Generic, Ae.ToJSON)



{-
JSON reply for a Text-To-Speech request:
{
  "candidates": [
    {
      "content": {
        "parts": [
          {
            "inlineData": {
              "mimeType": "audio/L16;codec=pcm;rate=24000",
              "data": "data..."
            }
          }
        ],
        "role": "model"
      },
      "finishReason": "STOP",
      "index": 0
    }
  ],
  "usageMetadata": {
    "promptTokenCount": 4,
    "candidatesTokenCount": 33,
    "totalTokenCount": 37,
    "promptTokensDetails": [
      {
        "modality": "TEXT",
        "tokenCount": 4
      }
    ],
    "candidatesTokensDetails": [
      {
        "modality": "AUDIO",
        "tokenCount": 33
      }
    ]
  },
  "modelVersion": "gemini-2.5-flash-preview-tts",
  "responseId": "BAt9aK2aCpm_vdIPoqnRsQ0"
}

-}

data ApiReplyC = ApiReplyC {
  candidates :: [CandidateC]
  , usageMetadata :: UsageMetadataC
  , modelVersion :: Text
  , responseId :: Text
  } deriving (Show, Generic, Ae.FromJSON)

data CandidateC = CandidateC {
  content :: ReplyContentC
  , finishReason :: Text
  , index :: Int
  } deriving (Show, Generic, Ae.FromJSON)

newtype ReplyContentC = ReplyContentC {
  parts :: [PartC]
  } deriving (Show, Generic, Ae.FromJSON)

data PartC =
  InlinePC { inlineData :: InlineDataC }
  | TextPC { text :: Text }
  deriving (Show, Generic)

instance Ae.FromJSON PartC where
  parseJSON = Ae.withObject "PartC" $ \o -> do
    inlineData <- o Ae..:? "inlineData"
    text <- o Ae..:? "text"
    case (inlineData, text) of
      (Just inlineData, Nothing) -> return $ InlinePC inlineData
      (Nothing, Just text) -> return $ TextPC text
      _ -> fail "@[ReplyPartC.parseJSON] illegal multiple parts in the same PartC."

instance Ae.ToJSON PartC where
  toEncoding = Ae.genericToEncoding (Ae.defaultOptions {Ae.sumEncoding = Ae.UntaggedValue })


data InlineDataC = InlineDataC {
  mimeType :: Text
  , data_ :: Text
  } deriving (Show, Generic)

instance Ae.FromJSON InlineDataC where
  parseJSON = Ae.withObject "InlineDataC" $ \o -> do
    mimeType <- o Ae..: "mimeType"
    data_ <- o Ae..: "data"
    return $ InlineDataC mimeType data_

instance Ae.ToJSON InlineDataC where
  toEncoding iD = Ae.pairs $
    "mimeType" Ae..= iD.mimeType
    <> "data" Ae..= iD.data_


data UsageMetadataC = UsageMetadataC {
  promptTokenCount :: Int
  , candidatesTokenCount :: Int
  , totalTokenCount :: Int
  , promptTokensDetails :: [PromptTokenDetailsC]
  , candidatesTokensDetails :: Maybe [CandidatesTokenDetailsC]
  , thoughtsTokenCount :: Maybe Int
  } deriving (Show, Generic, Ae.FromJSON)

data PromptTokenDetailsC = PromptTokenDetailsC {
  modality :: Text
  , tokenCount :: Int
  } deriving (Show, Generic, Ae.FromJSON)

data CandidatesTokenDetailsC = CandidatesTokenDetailsC {
  modality :: Text
  , tokenCount :: Int
  } deriving (Show, Generic, Ae.FromJSON)


-- possible function lab ["message", "chain_of_thought", "text_to_image", "text_to_speech", "text_to_video"]

handleRequestFor :: (ServiceContext, Hc.Manager) -> Rq.InvokeRequest ->  IO (Either String [ServiceResult])
handleRequestFor (srvCtxt, manager) request =
  let
    eiParams = extractInvokeItems request
  in
  case srvCtxt.functionD.label of
    "message" ->
      case eiParams of
        Left err ->
          pure $ Left "@[handleRequestFor] GoogleAI.text_to_speech err: need 'voice' and 'model' parameters."
        Right (params, content) ->
          message (srvCtxt, manager) params content
    "chain_of_thought" ->
      pure $ Left "@[handleRequestFor] Gemini: chain_of_thought is not implemented"
    "text_to_image" ->
      case eiParams of
        Left err ->
          pure $ Left "@[handleRequestFor] GoogleAI.text_to_speech err: need 'voice' and 'model' parameters."
        Right (params, content) ->
          textToImage (srvCtxt, manager) params content
    "text_to_video" ->
      pure $ Left "@[handleRequestFor] Gemini: text_to_video is not implemented"
    "text_to_speech" -> do
      case eiParams of
        Left err ->
          pure $ Left "@[handleRequestFor] GoogleAI.text_to_speech err: need 'voice' and 'model' parameters."
        Right (params, content) ->
          textToSpeech (srvCtxt, manager) params content
    _ ->
      pure . Left $ "@[handleRequestFor] ElevenLabs: function not supported: " <> unpack srvCtxt.functionD.label


message :: (ServiceContext, Hc.Manager) -> Mp.Map Text Text -> Text -> IO (Either String [ServiceResult])
message (srvCtxt, manager) params content =
{-
curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent" \
  -H "x-goog-api-key: $GEMINI_API_KEY" \
  -H 'Content-Type: application/json' \
  -X POST \
  -d '{
    "contents": [
      {
        "parts": [
          {
            "text": "How does AI work?"
          }
        ]
      }
    ]
  }'
-}
  let
    modelID = fromMaybe "gemini-2.5-flash" $ Mp.lookup "model" params
    messageReq = GenerationReqC {
        model = modelID
        , contents = [ContentC [TextPC content]]
        , generationConfig = Nothing
      }
  in do
  baseRequest <- Hc.parseRequest $ "https://generativelanguage.googleapis.com/v1beta/models/" <> unpack modelID <> ":generateContent"
  let
    request = baseRequest {
        Hc.method = "POST"
        , Hc.requestHeaders = [("x-goog-api-key", srvCtxt.apiKey), ("Content-Type", "application/json")]
        , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode messageReq
      }
  -- putStrLn $ "Request: " <> show request
  rezA <- Hc.httpLbs request manager
  case Hs.statusCode rezA.responseStatus of
    200 -> do
      case Ae.eitherDecode rezA.responseBody :: Either String ApiReplyC of
        Left err ->
          pure $ Left $ "@[handleRequestFor] Gemini/message err: " <> err
        Right response -> do
          let
            candidate0 = head response.candidates
            parts0 = head candidate0.content.parts
          case parts0 of
            TextPC text ->
              pure . Right $ [TextReplySR MarkdownRK text]
            _ ->
              pure $ Left $ "@[handleRequestFor] multiple parts in the same ReplyPartC: " <> show rezA.responseBody
    _ -> pure $ Left $ "@[handleRequestFor] http err: " <> show (Hs.statusCode rezA.responseStatus) <> "\n body: " <> (unpack . T.decodeUtf8 . Lbs.toStrict) rezA.responseBody


textToImage :: (ServiceContext, Hc.Manager) -> Mp.Map Text Text -> Text -> IO (Either String [ServiceResult])
textToImage (srvCtxt, manager) params content =
  let
    modelID = fromMaybe "gemini-2.0-flash-preview-image-generation" $ Mp.lookup "model" params
    speechReq = GenerationReqC {
      model = modelID
      , contents = [ContentC [TextPC content]]
      , generationConfig = Just $ GenerationConfigC {
          responseModalities = ["IMAGE", "TEXT"]
            , speechConfig = Nothing
            }
      }
  in do
  baseRequest <- Hc.parseRequest $ "https://generativelanguage.googleapis.com/v1beta/models/" <> unpack modelID <> ":generateContent"
  let
    request = baseRequest {
        Hc.method = "POST"
        , Hc.requestHeaders = [("x-goog-api-key", srvCtxt.apiKey), ("Content-Type", "application/json")]
        , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode speechReq
      }
  -- putStrLn $ "Request: " <> show request
  rezA <- Hc.httpLbs request manager
  case Hs.statusCode rezA.responseStatus of
    200 -> do
      case Ae.eitherDecode rezA.responseBody :: Either String ApiReplyC of
        Left err ->
          pure $ Left $ "@[handleRequestFor] Gemini/text_to_image err: " <> err
        Right response ->
          saveData srvCtxt response "Gemini/TTI"
    _ -> pure $ Left $ "@[handleRequestFor] http err: " <> show (Hs.statusCode rezA.responseStatus) <> "\n body: " <> (unpack . T.decodeUtf8 . Lbs.toStrict) rezA.responseBody


textToSpeech :: (ServiceContext, Hc.Manager) -> Mp.Map Text Text -> Text -> IO (Either String [ServiceResult])
textToSpeech (srvCtxt, manager) params content =
  let
    voiceID = fromMaybe "Iapetus" $ Mp.lookup "voice" params
    modelID = fromMaybe "gemini-2.5-flash-preview-tts" $ Mp.lookup "model" params
    speechReq = GenerationReqC {
      model = modelID
      , contents = [ContentC [TextPC content]]
      , generationConfig = Just $ GenerationConfigC {
          responseModalities = ["AUDIO"]
          , speechConfig = Just SpeechConfigC {
              voiceConfig = VoiceConfigC {
                prebuiltVoiceConfig = PrebuiltVoiceConfigC {
                  voiceName = voiceID
                  }
                }
              }
            }
      }
  in do
  baseRequest <- Hc.parseRequest $ "https://generativelanguage.googleapis.com/v1beta/models/" <> unpack modelID <> ":generateContent"
  let
    request = baseRequest {
        Hc.method = "POST"
        , Hc.requestHeaders = [("x-goog-api-key", srvCtxt.apiKey), ("Content-Type", "application/json")]
        , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode speechReq
      }
  -- putStrLn $ "Request: " <> show request
  rezA <- Hc.httpLbs request manager
  case Hs.statusCode rezA.responseStatus of
    200 -> do
      case Ae.eitherDecode rezA.responseBody :: Either String ApiReplyC of
        Left err ->
          pure $ Left $ "@[handleRequestFor] Gemini/text_to_speech err: " <> err
        Right response -> do
          saveData srvCtxt response "Gemini/TTS"
    _ -> pure $ Left $ "@[handleRequestFor] http err: " <> show (Hs.statusCode rezA.responseStatus) <> "\n body: " <> (unpack . T.decodeUtf8 . Lbs.toStrict) rezA.responseBody


saveData :: ServiceContext -> ApiReplyC -> Text -> IO (Either String [ServiceResult])
saveData srvCtxt response fctType =
  case response.candidates of
  [] ->
    pure $ Left $ "@[saveData] save data err: no candidates."
  candidate : _ ->
    if L.null candidate.content.parts then
      pure $ Left $ "@[saveData] save data err: no parts."
    else do
      eiRezB <- mapM (\part ->
        case part of
          TextPC text ->
            pure . Right $ TextReplySR MarkdownRK text
          InlinePC inlineData -> do
            newID <- Uu.nextRandom
            let
              newAsset = S3.prepareNewAsset newID fctType inlineData.mimeType 
            -- putStrLn $ "@[saveData] parts0.inlineData.data_: " <> show parts0.inlineData.data_
            rezA <- S3.insertNewAssetFromB64 srvCtxt.dbPool srvCtxt.s3Conn inlineData.data_ newAsset
            case rezA of
              Left err ->
                let
                  errMsg = "@[saveData] Error inserting asset: " <> err
                in do
                putStrLn errMsg
                pure $ Left errMsg
              Right updAsset -> do
                putStrLn $ "@[saveData] Asset inserted: " <> show updAsset
                pure . Right $ AssetSR updAsset
          _ -> pure $ Left $ "@[saveData] illegal part: " <> show part
        ) candidate.content.parts
      case Ei.lefts eiRezB of
        [] ->
          let 
            rezValues = Ei.rights eiRezB
          in do
          pure . Right $ rezValues
        errs ->
          pure $ Left $ "@[saveData] saving candidates err: " <> show errs
 