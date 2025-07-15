module Service.Types where

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vc

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae


data TopDescription = TopDescription {
    id :: Int32
    , eid :: UUID
    , name :: Text
    , description :: Text
    , functions :: Vc.Vector FunctionDescription
  }
  deriving (Show)


-- uid eid service_fk label pricingRule privilegeRule endpoint category
data FunctionDescription = FunctionDescription {
    id :: Int32
    , eid :: UUID
    , label :: Text
    , pricingRule :: Text
    , privilegeRule :: Text
    , endpoint :: Text
    , category :: Vc.Vector Text
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


data ServiceContext = ServiceContext {
    id :: UUID
    , apiKey :: Bs.ByteString
  }
  deriving (Show)
