{-# LANGUAGE QuasiQuotes #-}

module Assets.Storage where

import Data.Int (Int32, Int64)
import Data.Text (Text, pack)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH

import qualified Network.HTTP.Client.Conduit as Hcc
import qualified Network.HTTP.Types.Status as Hs

import qualified Assets.S3Ops as Ops
import Assets.Types


{-
create table if not exists Asset (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
  , name text
  , description text
  , contentType varchar(255)
  , size bigint not null
  , version int not null default 1
  , notes text
);
-}

prepareNewAsset :: Uu.UUID -> Text -> Text -> Asset
prepareNewAsset eid notes contentType = Asset {
  name = Nothing
  , uid = Nothing
  , eid = eid
  , description = Nothing
  , contentType = contentType
  , size = 0
  , version = 1
  , notes = Just notes
  }


insertNewAsset :: Pool -> S3Conn -> Hcc.Manager -> Hcc.Request -> Asset -> IO (Either String Asset)
insertNewAsset pgPool s3Conn manager request asset = do
  rezA <- Ops.streamHttpResponseToS3 s3Conn manager request (pack . Uu.toString $ asset.eid)
  case rezA of
    Left err ->
      pure . Left $ "@[insertNewAsset] streamHttpResponseToS3 err: " <> show err
    Right aSize -> do
      rezB <- use pgPool $ statement (asset.name, asset.eid, asset.description, asset.contentType, aSize, asset.notes) [TH.singletonStatement|
        insert into Asset
          (name, eid, description, contentType, size, notes)
        values
          ($1::text?, $2::uuid, $3::text?, $4::text, $5::bigint, $6::text?)
        returning uid::int4
      |]
      case rezB of
        Left err ->
          pure . Left $ "@[insertAsset] err: " <> show err
        Right anID ->
          pure . Right $ asset { uid = Just anID, size = aSize }
