{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module DB.Opers where

import Control.Monad.IO.Class (liftIO)

import Data.Int (Int16, Int32, Int64)
import Data.Maybe (isJust)
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


serializeInvocation :: Pool -> Rq.InvokeRequest -> IO (Either String Rr.InvokeResponse)
serializeInvocation pgPool request = do
  Right <$> Rr.fakeServiceResponse (Ae.toJSON $ TestTranz "123" "abc") "OK"