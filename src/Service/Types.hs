module Service.Types where

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae


data TopDescription = TopDescription {
    id :: UUID
    , name :: Text
    , description :: Text
  }
  deriving (Show)


data FunctionDescription = FunctionDescription {
    id :: UUID
    , name :: Text
    , description :: Text
  }
  deriving (Show)


data ComplexRequest = ComplexRequest {
    id :: UUID
    , components :: [ RequestComponent ]
    , startAt :: UTCTime
    , destination :: UUID
    , account :: UserAccount
    , context :: ServiceContext
  }

data RequestComponent =
  PromptRC Text
  | FunctionCallRC FunctionCall
  | AssetRC UUID


data FunctionCall = FunctionCall {
    name :: Text
    , parameters :: [Parameter]
  }
  deriving (Show)


data Parameter = Parameter {
    name :: Text
    , value :: Ae.Value
  }
  deriving (Show)


data MoneyAmount = MoneyAmount {
    i :: Int       -- Integral part
    , f :: Int     -- Fractional part
    , currency :: Text
  }
  deriving (Show)


data UserAccount = UserAccount {
    id :: UUID
    , name :: Text
    , balance :: MoneyAmount
  }
  deriving (Show)


newtype ServiceContext = ServiceContext {
    id :: UUID
  }
  deriving (Show)
