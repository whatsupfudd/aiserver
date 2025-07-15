{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds      #-}

module Api.RequestTypes where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)

import Servant.API (NoContent)


data LoginForm = LoginForm {
    username :: Text
    , secret :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


newtype LoginRequest = SimpleLR LoginForm


-- | 2. Client management
data UpdatePassword = UpdatePassword { 
    username :: Text
  , oldSecret :: Text
  , newSecret :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


newtype UpdatePreferences = UpdatePreferences { 
    preferences :: Value
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- | 3. Repeat
data RepeatRequest = RepeatRequest { 
    operationId :: UUID
  , service :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OperationStatus = OperationStatus { 
    requestId :: UUID
  , status :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | 4. Service invocation
data ServiceRequest = ServiceRequest { 
    service :: UUID
  , payload :: Value
  , files :: [UUID]
  , references :: [UUID]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)


-- | 5. Storage
data StoragePut = StoragePut { 
    name :: Text
  , mimeType :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data StorageMeta = StorageMeta { 
    itemId :: UUID
  , location :: Text
  , created :: UTCTime
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | 6. Admin
data ServiceDescriptor = ServiceDescriptor { 
    id :: UUID
  , name :: Text
  , costPerCall :: Double
  , schema :: Value
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data QuotaUpdate = QuotaUpdate { 
    clientId :: UUID
  , quota :: Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data QuotaInfo = QuotaInfo {
    clientId :: UUID
  , used :: Int
  , remaining :: Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ClientCreate = ClientCreate { 
    name :: Text
  , email :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

