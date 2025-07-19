{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module DB.Opers where

import Control.Monad.IO.Class (liftIO)

import Data.Int (Int16, Int32, Int64)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vc
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH

import qualified Data.Aeson as Ae

import Api.Types (ClientInfo (..))

import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr


checkValidClient :: Pool -> ClientInfo -> IO (Either String Bool)
checkValidClient dbPool clientInfo = do
  rezA <- use dbPool $ statement clientInfo.uid [TH.maybeStatement|
    select 1::int4, count(b.reason)::int4
    from cconnection a
    left join ccancel b on a.uid = b.connection_fk
    where a.uid = $1::int4
  |]
  case rezA of
    Left err -> do
      pure . Left $ "@[checkValidClient] err: " <> show err
    Right (Just (1, 0)) ->
      pure . Right $ True
    Right _ ->
      pure . Right $ False


authUserFromNameSecret :: Pool -> (Text, Text) -> IO (Either String (Maybe (Int32, UUID)))
authUserFromNameSecret dbPool (name, secret) = do
  rezA <- use dbPool $ statement (name, secret) [TH.maybeStatement|
    select a.uid::int4, a.eid::uuid
    from account a
    join authentication b on b.account_fk = a.uid
    where a.name = $1::text and b.method = 1 and b.secret = $2::text
  |]
  case rezA of
    Left err -> do
      pure . Left $ "@[authUserFromNameSecret] select err: " <> show err
    Right (Just ids) -> do
      pure . Right $ Just ids
    Right Nothing ->
      pure . Right $ Nothing


startNewConnection :: Pool -> Int32 -> Text -> IO (Either String Int32)
startNewConnection dbPool accountID token = do
  rezA <- use dbPool $ statement (accountID, token) [TH.singletonStatement|
    insert into cconnection (account_fk, token) values ($1::int4, $2::text)
    returning uid::int4
  |]
  case rezA of
    Left err -> do
      pure . Left $ "@[startNewConnection] err: " <> show err
    Right uid ->
      pure . Right $ uid


data TestTranz = TestTranz {
  aField :: Text
  , bField :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass Ae.ToJSON


{-
create table if not exists crequest (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , account_fk int not null references account(uid)
  , function_eid uuid not null
  , params jsonb not null
  , created_at timestamp not null default now()
);

-}

serializeInvocation :: Pool -> Int32 -> Rq.InvokeRequest -> Maybe Ae.Value -> IO (Either String Rr.InvokeResponse)
serializeInvocation pgPool accountID request mbResult = do
  rezA <- use pgPool $ statement (accountID, request.function, request.parameters) [TH.singletonStatement|
    insert into crequest (account_fk, function_eid, params) values ($1::int4, $2::uuid, $3::jsonb)
    returning uid::int4, eid::uuid
  |]
  case rezA of
    Left err ->
      pure . Left $ "@[serializeInvocation] err: " <> show err
    Right (uid, eid) ->
      pure . Right $ Rr.InvokeResponse {
        requestID = uid
        , requestEId = eid
        , contextID = 0
        , contextEId = eid
        , status = "OK"
        , result = fromMaybe Ae.Null mbResult
      }


data TransactionStatus =
    CompletedTS
  | FailedTS Text

{-
create table if not exists ReqExec (
  uid serial primary key
  , crequest_fk int not null references crequest(uid)
  , created_at timestamp not null default now()
  , kind int not null default 1     -- 1: submission, 2: reply, 3: replay, 4: timeout, 5: error.
  , notes text
);
-}

endTransaction :: Pool -> Rr.InvokeResponse -> TransactionStatus -> IO (Either String Int32)
endTransaction pgPool invResp status =
  let
    (kind, mbNote) = case status of
      CompletedTS -> (1, Nothing)
      FailedTS errMsg -> (5, Just errMsg)
  in do
  rezA <- use pgPool $ statement (invResp.requestID, kind ,mbNote) [TH.singletonStatement|
      insert into ReqExec (crequest_fk, kind, notes) values ($1::int4, $2::int4, $3::text?)
      returning uid::int4
    |]
  case rezA of
    Left err ->
      pure . Left $ "@[endTransaction] err: " <> show err
    Right anID ->
      pure $ Right anID

{-
create table if not exists cresponse (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , crequest_fk int not null references crequest(uid)
  , created_at timestamp not null default now()
  , kind int not null default 1     -- 1: plain text, 2: json, 3: base64, 4: markdown, 5: reference to asset.
  , content text
);
-}

data ResponseKind =
  PlainTextRK
  | JsonRK
  | Base64RK
  | MarkdownRK
  | AssetRK

rkToInt32 :: ResponseKind -> Int32
rkToInt32 PlainTextRK = 1
rkToInt32 JsonRK = 2
rkToInt32 Base64RK = 3
rkToInt32 MarkdownRK = 4
rkToInt32 AssetRK = 5

int32ToRk :: Int32 -> ResponseKind
int32ToRk 1 = PlainTextRK
int32ToRk 2 = JsonRK
int32ToRk 3 = Base64RK
int32ToRk 4 = MarkdownRK
int32ToRk 5 = AssetRK


insertResponse :: Pool -> Rr.InvokeResponse -> ResponseKind -> Maybe Text -> IO (Either String (Int32, UUID))
insertResponse pgPool invR kind mbResult = do
  rezA <- use pgPool $ statement (invR.requestID, rkToInt32 kind, mbResult) [TH.singletonStatement|
    insert into cresponse
      (crequest_fk, kind, content)
      values ($1::int4, $2::int4, $3::text?)
    returning uid::int4, eid::uuid
  |]
  case rezA of
    Left err ->
      pure . Left $ "@[insertResponse] err: " <> show err
    Right (uid, eid) ->
      pure $ Right (uid, eid)

getResponse :: Pool -> UUID -> IO (Either String Rr.ResponseResponse)
getResponse pgPool requestEID = do
  rezA <- use pgPool $ statement requestEID [TH.maybeStatement|
    select
      a.uid::int4, a.eid::uuid, a.kind::int4, a.content::text?, c.eid::uuid?, c.size::int8?
    from cresponse a
     join crequest d on a.crequest_fk = d.uid
     left join assetresponselink b on a.uid = b.response_fk
     left join asset c on b.asset_fk = c.uid
    where d.eid = $1::uuid
  |]
  case rezA of
    Left err ->
      pure . Left $ "@[getResponse] err: " <> show err
    Right Nothing -> pure . Right $ Rr.ResponseResponse {
        responseEId = requestEID
        , result = Rr.NoResponseYetRK
      }
    Right (Just (uid, eid, kind, mbContent, mbAssetEid, mbAssetSize)) ->
      let
        responseContent = if kind `elem` [1,2,3,4] then
            case mbContent of
              Nothing -> Rr.NoResponseYetRK
              Just content ->
                let
                  tBlockFormat = case int32ToRk kind of
                    PlainTextRK -> Rr.PlainTextTF
                    JsonRK -> Rr.JsonTF
                    Base64RK -> Rr.Base64TF
                    MarkdownRK -> Rr.MarkdownTF
                    _ -> Rr.PlainTextTF
                in
                Rr.TextBlockRK tBlockFormat content
          else
            case mbAssetEid of
              -- TODO: manage this better, the response should exist only after the asset has been created
              Nothing -> Rr.NoResponseYetRK
              Just assetEid ->
                Rr.AssetRK mbContent mbAssetSize assetEid
      in
      pure . Right $ Rr.ResponseResponse {
        responseEId = eid
        , result = responseContent
      }
linkAssetToResponse :: Pool -> Int32 -> Int32 -> IO (Either String Int32)
linkAssetToResponse pgPool responseID assetID = do
  rezA <- use pgPool $ statement (responseID, assetID) [TH.singletonStatement|
    insert into assetResponseLink (response_fk, asset_fk) values ($1::int4, $2::int4)
    returning uid::int4
  |]
  case rezA of
    Left err ->
      pure . Left $ "@[linkAssetToResponse] err: " <> show err
    Right anID ->
      pure $ Right anID