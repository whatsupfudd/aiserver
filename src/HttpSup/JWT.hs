module HttpSup.JWT where

import Control.Lens ((?~), (^.), view, set)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
-- import Crypto.JOSE.JWA.JWS (Alg (ES256))
{-
import Crypto.JOSE.JWK (AsPublicKey (asPublicKey), Crv (P_256),
                                       JWK, JWKAlg (JWSAlg),
                                       KeyMaterialGenParam (ECGenParam),
                                       KeyOp (Sign, Verify), KeyUse (Sig),
                                       MonadRandom, genJWK, jwkAlg, jwkKeyOps,
                                       jwkUse)
-}
import Crypto.JWT
import qualified Crypto.JWT as Jose
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as DT

import Servant.Auth.Server


generateKeyPair :: MonadRandom m => m JWK
generateKeyPair = do
  k <- genJWK . ECGenParam $ P_256
  return $ k
    & jwkAlg ?~ JWSAlg ES256
    & jwkKeyOps ?~ [Sign, Verify]
    & jwkUse ?~ Sig


tmpJWK :: IO JWK
tmpJWK = do
  jwk <- genJWK (RSAGenParam (4096 `div` 8))
  -- let
  --  h = view thumbprint jwk :: Digest SHA256
  --  kid = view (re (base64url . digest) . utf8) h
  pure $ set jwkKid Nothing jwk


-- | Generate jwk and public version according to kyrosid specs.
generateKeyPairIO :: FilePath -> IO ()
generateKeyPairIO path = do
  keyPair <- generateKeyPair
  let mbPubJWK = keyPair ^. asPublicKey
  case mbPubJWK of
    Nothing -> fail "Public JWK generation error"
    Just pubJWK ->
      encodeFile (path <> ".pub") pubJWK
  encodeFile path keyPair


-- | Read JWK from file
readJWK :: FilePath -> IO JWK
readJWK path = do
  eJWK <- eitherDecodeFileStrict path
  case eJWK of
    Left e -> fail e
    Right keyPair -> pure keyPair


verifyJWT' :: FromJWT a => JWTSettings -> BS.ByteString -> IO (Either Text a)
verifyJWT' jwtCfg input = do
  verifiedJWT <- liftIO $ runExceptT . withExceptT formJWTError $ do
    unverifiedJWT <- Jose.decodeCompact (BSL.fromStrict input)
    valKeys <- liftIO $ validationKeys jwtCfg
    Jose.verifyClaims
      (jwtSettingsToJwtValidationSettings jwtCfg)
      valKeys
      unverifiedJWT

  let eitherResult = verifiedJWT >>= decodeJWT

  return eitherResult
  where
    formJWTError :: JWTError -> Text
    formJWTError = DT.pack . show


jwtSettingsToJwtValidationSettings :: JWTSettings -> Jose.JWTValidationSettings
jwtSettingsToJwtValidationSettings s =
  defaultJWTValidationSettings (toBool <$> audienceMatches s)
  where
    toBool Matches = True
    toBool DoesNotMatch = False
