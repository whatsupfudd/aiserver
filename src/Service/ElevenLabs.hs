module Service.ElevenLabs where

import qualified Data.ByteString.Lazy as Lbs
import Data.Text (Text)

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Types.Status as Hs
import qualified Data.Aeson as Ae

import Service.Types


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
-}

handleRequestFor :: ServiceContext -> Text ->  IO (Either Text ())
handleRequestFor srvCtxt aFunction =
  case aFunction of
    "text_to_speech" -> do
      let requestObject = Ae.object [
            "text" Ae..= ("It's fascinating, isn't it? The sources jump right to it, a vibrant entrepreneurial landscape. That's the recurrent driver. And look, it's not just about creating jobs, though that's obviously a huge benefit. It's more about fostering this environment where value is constantly being created, where innovation isn't just, you know, a buzzword, but it's happening all the time, new solutions popping up. But here's the tricky part. Lots of governments, organizations, worldwide, they've tried to build these ecosystems, and frankly, most haven't really succeeded. Why? Because, as the source material puts it, fostering entrepreneurship at scale is far from being a known and understood topic. That real practical experience, that deep understanding of how to actually make it work, it's incredibly rare, hard to find." :: Text)
            , "model_id" Ae..= ("eleven_multilingual_v2" :: Text)
            ]
          modelID = "JBFqnCBsd6RMkjVDRZzb"
          outputFormat = "mp3_44100_128"
      manager <- Hc.newManager Hc.defaultManagerSettings
      initialRequest <- Hc.parseRequest $ "https://api.elevenlabs.io/v1/text-to-speech/" <> modelID <> "?output_format=" <> outputFormat
      let request = initialRequest {
          Hc.method = "POST"
          , Hc.requestBody = Hc.RequestBodyLBS $ Ae.encode requestObject
          , Hc.requestHeaders = [("xi-api-key", srvCtxt.apiKey), ("Content-Type", "application/json")]
          }
      response <- Hc.httpLbs request manager
      putStrLn $ "The status code was: " <> show response.responseStatus.statusCode
      Lbs.writeFile "/tmp/response.mp3" response.responseBody
      pure $ Right ()
    "list_voices" -> do
      manager <- Hc.newManager Hc.defaultManagerSettings
      initialRequest <- Hc.parseRequest "https://api.elevenlabs.io/v2/voices"
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
