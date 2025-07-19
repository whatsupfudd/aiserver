{-# LANGUAGE LambdaCase #-}

module Service.Opers where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vc
import Data.UUID (UUID)

import Hasql.Pool (Pool)

import qualified Data.Aeson as Ae

import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr

import qualified DB.Opers as Db
import qualified Api.Types as At
import qualified Assets.Types as Ast

import qualified Service.DbOps as Sdb
import Service.Types

import qualified Service.ElevenLabs as XiL


processInvocation :: At.AppEnv -> At.ClientInfo -> Rq.InvokeRequest -> IO (Either String Rr.InvokeResponse)
processInvocation appEnv clientInfo request = do
  rezA <- Db.serializeInvocation appEnv.pgPool_Ctxt clientInfo.uid request Nothing
  case rezA of
    Left errMsg ->
      let
        errMsg = "@[processInvocation] serializeRequest err: " <> errMsg
      in do
        putStrLn errMsg
        pure . Left $ errMsg
    Right tranz -> do
      putStrLn $ "@[processInvocation] serialized request: " <> show tranz
      case appEnv.s3Storage_Ctxt of
        Nothing ->
          let
            errMsg = "@[processInvocation] no s3Conn"
          in do
          putStrLn $ unpack errMsg
          Db.endTransaction appEnv.pgPool_Ctxt tranz (Db.FailedTS errMsg)
        Just s3Conn ->
          buildServiceContext appEnv.serviceDefs_Ctxt appEnv.pgPool_Ctxt s3Conn request.function >>= \case
            Left err ->
              let
                errMsg = "@[processInvocation] buildServiceContext err: " <> err
              in do
              putStrLn errMsg
              Db.endTransaction appEnv.pgPool_Ctxt tranz (Db.FailedTS (pack errMsg))
            Right srvCtxt -> do
              spanInvocation srvCtxt request tranz
              pure $ Right 0
      pure . Right $ tranz


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


buildServiceContext :: Mp.Map Text TopDescription -> Pool -> Ast.S3Conn -> UUID -> IO (Either String ServiceContext)
buildServiceContext descriptions dbPool s3Conn targetFct = do
  case findServiceForFunction descriptions targetFct of
    Nothing -> pure . Left $ "@[buildServiceContext] no service for function: " <> show targetFct
    Just (serviceDescr, fctDescr) -> do
      putStrLn $ "@[buildServiceContext] serviceName: " <> show serviceDescr.name
      eiAuthRez <- Sdb.getAuthForServiceID dbPool serviceDescr.id "-system"
      case eiAuthRez of
        Left err -> do
          putStrLn $ "@[buildServiceContext] getAuthForServiceID err: " <> err
          pure . Left $ err
        Right mbAuth -> do
          case mbAuth of
            Nothing ->
              let
                errMsg = "@[buildServiceContext] no auth for: " <> show serviceDescr.id
              in do
              putStrLn errMsg
              pure . Left $ errMsg
            Just (idLabel, secret, method) ->
              pure . Right $ ServiceContext {
                  label = idLabel
                  , apiKey = T.encodeUtf8 secret
                  , serviceD = serviceDescr
                  , functionD = fctDescr
                  , dbPool = dbPool
                  , s3Conn = s3Conn
                }


spanInvocation :: ServiceContext -> Rq.InvokeRequest -> Rr.InvokeResponse -> IO ()
spanInvocation srvCtxt request tranz = do
  void . forkIO $ do
    putStrLn $ "@[spanInvocation] request: " <> show request
    putStrLn $ "@[spanInvocation] tranz: " <> show tranz
    -- rezA <- Sdb.getServiceForFunction dbPool request.function
    rezA <- case srvCtxt.serviceD.name of
      "claudeai" -> do
        putStrLn "@[spanInvocation] claudeai."
        pure $ Left "@[spanInvocation] claudeai is not implemented."
      "deepseek" -> do
        putStrLn "@[spanInvocation] deepseek."
        pure $ Left "@[spanInvocation] deepseek is not implemented."
      "elevenlabs" -> do
        XiL.spanInvocation srvCtxt request tranz
      "flux" -> do
        putStrLn "@[spanInvocation] flux."
        pure $ Left "@[spanInvocation] flux is not implemented."
      "gemini" -> do
        putStrLn "@[spanInvocation] gemini."
        pure $ Left "@[spanInvocation] gemini is not implemented."
      "groq" -> do
        putStrLn "@[spanInvocation] groq."
        pure $ Left "@[spanInvocation] groq is not implemented."
      "kimi" -> do
        putStrLn "@[spanInvocation] kimi."
        pure $ Left "@[spanInvocation] kimi is not implemented."
      "llama" -> do
        putStrLn "@[spanInvocation] llama."
        pure $ Left "@[spanInvocation] llama is not implemented."
      "openai" -> do
        putStrLn "@[spanInvocation] openai."
        pure $ Left "@[spanInvocation] openai is not implemented."
      "qwen" -> do
        putStrLn "@[spanInvocation] qwen."
        pure $ Left "@[spanInvocation] qwen is not implemented."
      _ -> do
        putStrLn $ "@[spanInvocation] unknown handler for: " <> show srvCtxt.serviceD.name
        pure $ Left $ "@[spanInvocation] unknown handler for: " <> show srvCtxt.serviceD.name
    case rezA of
      Left err -> do
        putStrLn $ "@[spanInvocation] spanInvocation err: " <> err
      Right result -> do
        rezB <- case result of
          AssetSR asset -> do
            putStrLn "@[spanInvocation] AssetSR"
            case asset.uid of
              Nothing -> do
                Db.endTransaction srvCtxt.dbPool tranz (Db.FailedTS "@[spanInvocation] AssetSR: no uid")
                pure $ Left "@[spanInvocation] AssetSR: no uid"
              Just assetID -> do
                Db.insertResponse srvCtxt.dbPool tranz Db.AssetRK Nothing >>= \case
                  Left err -> do
                    putStrLn $ "@[spanInvocation] insertResponse (asset) err: " <> err
                    pure . Left $ "@[spanInvocation] insertResponse (asset) err: " <> err
                  Right (uid, eid) -> do
                    Db.linkAssetToResponse srvCtxt.dbPool uid assetID >>= \case
                      Left err -> do
                        putStrLn $ "@[spanInvocation] linkAssetToResponse err: " <> err
                        pure . Left $ "@[spanInvocation] linkAssetToResponse err: " <> err
                      Right _ -> pure $ Right (uid, eid)
          TextReplySR rKind someText -> do
            putStrLn "@[spanInvocation] TextReplySR"
            Db.insertResponse srvCtxt.dbPool tranz (replyKindToDbKind rKind) (Just someText)
          ComplexReplySR aValue-> do
            putStrLn "@[spanInvocation] ComplexReplySR"
            Db.insertResponse srvCtxt.dbPool tranz Db.JsonRK (Just . T.decodeUtf8 . Lbs.toStrict $ Ae.encode aValue)
        case rezB of
          Left err -> do
            putStrLn $ "@[spanInvocation] insertResponse err: " <> err
          Right _ -> do
            Db.endTransaction srvCtxt.dbPool tranz Db.CompletedTS
            pure ()


replyKindToDbKind :: ReplyKind -> Db.ResponseKind
replyKindToDbKind aRKind =
  case aRKind of
    PlainTextRK -> Db.PlainTextRK
    JsonRK -> Db.JsonRK
    Base64RK -> Db.Base64RK
    MarkdownRK -> Db.MarkdownRK
    _ -> Db.PlainTextRK