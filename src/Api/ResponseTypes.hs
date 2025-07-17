{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Api.ResponseTypes where

import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock (UTCTime (..), getCurrentTime)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Api.Types (SessionItems)


data LoginResult =
  ErrorLR String
  | UnauthorizedLR String
  | AuthenticatedLR SessionItems
  deriving stock Generic
  deriving anyclass (Ae.ToJSON)


newtype ReplyClientRequest = ReplyClientRequest {
    tmpResult :: Text -- Lbs.ByteString
  }
  deriving stock Generic
  deriving anyclass Ae.ToJSON


data ClientServiceDescriptor = ClientServiceDescriptor {
    id :: UUID
  , name :: Text
  , descriptions :: Text
  } deriving (Show, Eq, Generic, Ae.FromJSON, Ae.ToJSON)


data InvokeResponse = InvokeResponse { 
    requestID :: Int32
  , requestEId :: UUID
  , contextID :: Int32
  , contextEId :: UUID
  , status :: Text
  , result :: Ae.Value
  }
  deriving (Show, Eq, Generic)

instance Ae.ToJSON InvokeResponse where
   toEncoding invR = Ae.pairs (
        "requestEId" Ae..= invR.requestEId
      <> "contextEId" Ae..= invR.contextEId
      <> "status" Ae..= invR.status
      <> "result" Ae..= invR.result
    )


fakeServiceResponse :: Ae.Value -> Text -> IO InvokeResponse
fakeServiceResponse result status = do
  moment <- getCurrentTime
  newUuid <- nextRandom
  pure $ InvokeResponse {
        requestID = 1
        , requestEId = newUuid
        , contextID = 1
        , contextEId = newUuid
        , result = result
        , status = status
      }