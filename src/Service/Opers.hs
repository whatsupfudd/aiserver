module Service.Opers where

import Control.Monad.IO.Class (liftIO)

import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vc
import Data.UUID (UUID)

import Hasql.Pool (Pool)

import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr

import qualified DB.Opers as Db
import qualified Service.DbOps as Sdb
import Service.Types (ServiceContext (..), TopDescription (..), FunctionDescription (..))
import qualified Service.ElevenLabs as XiL


findServiceForFunction :: Mp.Map Text TopDescription -> UUID -> Maybe (TopDescription, FunctionDescription)
findServiceForFunction srvDescr fctID =
  case Mp.elems srvDescr of
    [] -> Nothing
    (h:t) -> findServiceInList fctID h t
  where
  findServiceInList :: UUID -> TopDescription -> [TopDescription] -> Maybe (TopDescription, FunctionDescription)
  findServiceInList fctID target rest =
    case Vc.find (\fct -> fct.eid == fctID) target.functions of
      Nothing ->
        case rest of
          [] -> Nothing
          (h:t) -> findServiceInList fctID h t
      Just fctDescr -> Just (target, fctDescr)


spanInvocation :: Mp.Map Text TopDescription -> Pool -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO (Either String Int32)
spanInvocation srvCtxt dbPool request tranz = do
  liftIO $ putStrLn $ "@[spanInvocation] request: " <> show request
  liftIO $ putStrLn $ "@[spanInvocation] tranz: " <> show tranz
  -- rezA <- liftIO $ Sdb.getServiceForFunction dbPool request.function
  case findServiceForFunction srvCtxt request.function of
    Nothing -> pure . Left $ "no service for function: " <> show request.function
    Just (serviceDescr, fctDescr) -> do
      putStrLn $ "@[spanInvocation] serviceName: " <> show serviceDescr.name
      eiAuthRez <- Sdb.getAuthForServiceID dbPool serviceDescr.id "-system"
      case eiAuthRez of
        Left err -> do
          putStrLn $ "@[spanInvocation] getAuthForServiceID err: " <> err
          pure . Left $ err
        Right mbAuth -> do
          case mbAuth of
            Nothing ->
              let
                errMsg = "@[spanInvocation] no auth for: " <> show serviceDescr.id
              in do
              putStrLn errMsg
              pure . Left $ errMsg
            Just (idLabel, secret, method) ->
              let
                serviceDef = ServiceContext {
                  label = idLabel
                  , apiKey = T.encodeUtf8 secret
                }
              in do
              case serviceDescr.name of
                "claudeai" -> putStrLn "@[spanInvocation] claudeai."
                "deepseek" -> putStrLn "@[spanInvocation] deepseek."
                "elevenlabs" -> do
                  rez <- XiL.spanInvocation serviceDef dbPool fctDescr.label request tranz
                  case rez of
                    Left err -> do
                      putStrLn $ "@[spanInvocation] spanInvocation err: " <> err
                    Right _ -> pure ()
                "flux" -> putStrLn "@[spanInvocation] flux."
                "gemini" -> putStrLn "@[spanInvocation] gemini."
                "groq" -> putStrLn "@[spanInvocation] groq."
                "kimi" -> putStrLn "@[spanInvocation] kimi."
                "llama" -> putStrLn "@[spanInvocation] llama."
                "openai" -> putStrLn "@[spanInvocation] openai."
                "qwen" -> putStrLn "@[spanInvocation] qwen."
                _ -> putStrLn $ "@[spanInvocation] unknown handler for: " <> show serviceDescr.name
              pure . Right $ serviceDescr.id

