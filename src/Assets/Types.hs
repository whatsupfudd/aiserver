module Assets.Types where

import Data.Text (Text)

import qualified Network.Minio as Mn


data S3Config = S3Config {
    user :: Text
    , passwd :: Text
    , host :: Text
    , region :: Text
    , bucket :: Text
  }
  deriving (Show)

defaultS3Conf :: S3Config
defaultS3Conf =
  S3Config {
    user = "ubezwdy"
    , passwd = "pbezwdy"
    , host = "http://localhost:3900"
    , region = "garage"
    , bucket = "easywordy.1"
  }


data S3Conn = S3Conn {
    bucketCn :: Text
    , credentialsCn :: Mn.CredentialValue
    , connInfoCn :: Mn.ConnectInfo
  }
