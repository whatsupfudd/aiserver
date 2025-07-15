{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Api.ResponseTypes where

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


data ServiceResponse = ServiceResponse {
    requestId :: UUID
  , result    :: Ae.Value
  , created   :: UTCTime
  , status    :: Text
  } deriving (Show, Eq, Generic, Ae.FromJSON, Ae.ToJSON)


fakeServiceResponse :: Ae.Value -> Text -> IO ServiceResponse
fakeServiceResponse result status = do
  moment <- getCurrentTime
  newUuid <- nextRandom
  pure $ ServiceResponse {
        requestId = newUuid
        , result = result
        , created = moment
        , status = status
      }