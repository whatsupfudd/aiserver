{-# LANGUAGE DeriveGeneric #-}

module Service.Types where

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as Vc

import GHC.Generics (Generic)

import Hasql.Pool (Pool)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ak
import qualified Data.Aeson.Key as Ak

import qualified Api.RequestTypes as Rq

import qualified Assets.Types as S3



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
  deriving (Show)

data RequestComponent =
  PromptRC Text
  | FunctionCallRC FunctionCall
  | AssetRC UUID
  deriving (Show)


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
    label :: Text
    , serviceD :: TopDescription
    , functionD :: FunctionDescription
    , apiKey :: Bs.ByteString
    , dbPool :: Pool
    , s3Conn :: S3.S3Conn
  }

instance Show ServiceContext where
  show srvCtxt =
    "ServiceContext { label = " <> show srvCtxt.label
      <> ", apiKey = " <> show srvCtxt.apiKey
      <> ", s3Conn.bucket = " <> show srvCtxt.s3Conn.bucketCn
      <> " }"


data KnownServices =
  ClaudeAIIP
  | DeepSeekIP
  | ElevenLabsIP
  | FluxIP
  | GeminiIP
  | GroqIP
  | KimiIP
  | LlamaIP
  | OpenAIIP
  | QwenIP
  deriving (Show)


data InvokePacket = InvokePacket {
    service :: KnownServices
    , request :: ComplexRequest
  }
  deriving (Show)


data ReplyKind =
  PlainTextRK
  | JsonRK
  | Base64RK
  | MarkdownRK
  deriving (Show)

data ServiceResult =
  AssetSR S3.Asset
  | TextReplySR ReplyKind Text
  | ComplexReplySR Ae.Value
  deriving (Show)



data ParameterValue = ParameterValue {
    name :: Text
  , value :: Text
  } deriving (Show, Eq, Generic)

instance Ae.FromJSON ParameterValue where
  parseJSON (Ae.Object o) =
    let
      (label, value) = head $ Ak.toList o
      ident = case value of
        Ae.String s -> s
        Ae.Null -> ""
        _ -> "[ParameterValue.parseJSON] Invalid fct value: " <> (pack . show) value
    in
      pure (ParameterValue (pack . Ak.toString $ label) ident)


newtype ParamContainer = ParamContainer {
    params :: [ ParameterValue ]
  } deriving (Show, Eq, Generic)


instance Ae.FromJSON ParamContainer where
  parseJSON (Ae.Object o) =
    let
      paramList = [ ParameterValue (pack $ Ak.toString k) v | (k, Ae.String v) <- Ak.toList o ]
    in pure $ ParamContainer paramList
  parseJSON _ = fail "ParamContainer: Expected an object"

extractInvokeItems :: Rq.InvokeRequest -> Either String (Mp.Map Text Text, Text)
extractInvokeItems request = do
  case Ae.fromJSON request.parameters :: Ae.Result ParamContainer of
    Ae.Error err ->
      let
        errMsg = "@[extractInvokeItems] Error parsing parameters: " <> err
      in
      Left errMsg
    Ae.Success paramContainer ->
      let
        paramMap = Mp.fromList [ (p.name, p.value) | p <- paramContainer.params ]
      in
      case Ae.fromJSON request.content :: Ae.Result Text of
        Ae.Error err ->
          let
            errMsg = "@[extractInvokeItems] Error parsing content: " <> err
          in
            Left errMsg
        Ae.Success line ->
          Right (paramMap, line)
          