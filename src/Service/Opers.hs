{-# LANGUAGE LambdaCase #-}

module Service.Opers where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, foldM)

import qualified Data.ByteString.Lazy as Lbs
import qualified Data.Either as Ei
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Maybe (isNothing)
import Data.Text (Text, unpack, pack, toLower)
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
import qualified Service.Flux as Flx
import qualified Service.Anthropic as Ant
import qualified Service.GoogleAI as Gai

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
    rezA <- case toLower srvCtxt.serviceD.name of
      "anthropic" -> do
        Ant.spanInvocation srvCtxt request tranz
      "deepseek" -> do
        putStrLn "@[spanInvocation] deepseek."
        pure $ Left "@[spanInvocation] deepseek is not implemented."
      "elevenlabs" -> do
        XiL.spanInvocation srvCtxt request tranz
      "flux" -> do
        Flx.spanInvocation srvCtxt request tranz
      "googleai" -> do
        Gai.spanInvocation srvCtxt request tranz
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
        putStrLn $ "@[spanInvocation] service " <> show srvCtxt.serviceD.name <> " err: " <> err
      Right results ->
        let
          (assets, texts) = foldl (\(aAccum, tAccum) aResult ->
            case aResult of
              AssetSR asset -> (aResult :aAccum, tAccum)
              TextReplySR rKind someText -> (aAccum, aResult : tAccum)
              -- TODO: Deal with ComplexReplySR:
              _ -> (aAccum, tAccum)
            )  ([], []) results
        in do
        rezB <- case texts of
          [] ->
            mapM (\case
                AssetSR asset ->
                  saveAsset asset
                _ -> pure $ Left "@[spanInvocation] not an AssetSR in asset list."
              ) assets
          _ ->
            case assets of
              [] ->
                mapM (\case
                    TextReplySR rKind someText ->
                      Db.insertResponse srvCtxt.dbPool tranz (replyKindToDbKind rKind) (Just someText)
                    _ -> pure $ Left "@[spanInvocation] not a TextReplySR in text list."
                ) texts
              _ ->
                let
                  fullText = foldl (\accum aText ->
                    case aText of
                      TextReplySR rKind someText ->
                        if isNothing accum then Just someText else (<> someText) <$> accum
                      _ -> accum
                    ) Nothing texts
                in
                mapM (\case
                  AssetSR asset ->
                    saveAsset asset
                  _ -> pure $ Left "@[spanInvocation] not an AssetSR in asset list."
                  ) assets
        case Ei.lefts rezB of
          [] -> do
            Db.endTransaction srvCtxt.dbPool tranz Db.CompletedTS
          errs ->
            let
              errMsg = "@[spanInvocation] serialization of ServiceResults err: " <> show errs
            in do
            putStrLn errMsg
            Db.endTransaction srvCtxt.dbPool tranz (Db.FailedTS (pack errMsg))
        pure ()
  where
  saveAsset :: Ast.Asset -> IO (Either String (Int32, UUID))
  saveAsset anAsset = do
    putStrLn "@[spanInvocation] AssetSR"
    case anAsset.uid of
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
              Right _ -> pure . Right $ (uid, eid)

{-
        case rezB of
          Left err -> do
            putStrLn $ "@[spanInvocation] insertResponse err: " <> err
          Right _ -> do
            Db.endTransaction srvCtxt.dbPool tranz Db.CompletedTS
            pure ()

          case result of
            AssetSR asset ->
              putStrLn $ "@[spanInvocation] AssetSR: " <> show asset
            TextReplySR rKind someText -> do
              putStrLn "@[spanInvocation] TextReplySR"
              Db.insertResponse srvCtxt.dbPool tranz (replyKindToDbKind rKind) (Just someText)
            ComplexReplySR aValue-> do
              putStrLn "@[spanInvocation] ComplexReplySR"
              Db.insertResponse srvCtxt.dbPool tranz Db.JsonRK (Just . T.decodeUtf8 . Lbs.toStrict $ Ae.encode aValue)
-}

replyKindToDbKind :: ReplyKind -> Db.ResponseKind
replyKindToDbKind aRKind =
  case aRKind of
    PlainTextRK -> Db.PlainTextRK
    JsonRK -> Db.JsonRK
    Base64RK -> Db.Base64RK
    MarkdownRK -> Db.MarkdownRK
    _ -> Db.PlainTextRK