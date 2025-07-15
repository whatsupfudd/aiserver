module Api.Session where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock (secondsToDiffTime, DiffTime)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Servant.Auth.Server ( AuthResult (..)
            , BasicAuthData (..)
            , CookieSettings (..), SetCookie (..)
            , JWTSettings (..), ToJWT (..), makeJWT
          )

-- import Hasql.Pool (Pool, use)

import Api.Types (AppEnv (..), AIServerApp (..), ClientInfo (..), SessionItems (..), fakeClientInfo)
import Api.RequestTypes (LoginForm (..))
import Api.ResponseTypes (LoginResult (..))

-- import DB.User (getUserFromNameMd5, genNewSessionRow)


loginUser :: LoginForm -> AIServerApp LoginResult
loginUser request = do
  -- dbPool <- asks dbPool_Ctxt
  jwtInfo <- asks jwt_Ctxt
  -- TODO: extract the session duration from the config:
  -- eiRez <- authorizeNewSession dbPool jwtInfo request (secondsToDiffTime 86400)
  eiRez <- authorizeNewSession jwtInfo request (secondsToDiffTime 86400)
  case eiRez of
    Left errMsg ->
      pure . ErrorLR $ "@[loginReq] authorizeNewSession err: " <> errMsg
    Right authRez ->
      pure authRez


{-
authorizeNewSession :: Pool -> JWTSettings -> LoginForm -> DiffTime -> EasyVerseApp (Either String LoginResult)
authorizeNewSession dbPool jwtInfo lForm sessionDuration = do
  rezA <- liftIO $ use dbPool $ getUserFromNameMd5 (lForm.username, lForm.secret)
  case rezA of
    Left err -> pure . Left $ "@[authorizeNewSession] getUserFromNameMd5 err: " <> show err
    Right mbUserID ->
      case mbUserID of
        Nothing -> pure . Left $ "@[authorizeNewSession] unauthorized user: " <> unpack lForm.username <> "."
        Just userID -> do
          rezB <- liftIO $ use dbPool $ genNewSessionRow (userID, sessionDuration)
          case rezB of
            Left err -> pure . Left $ "@[authorizeNewSession] genNewSessionRow err:" <> show err
            Right (sessionID, expiry) -> do
              let clientInfo = ClientInfo sessionID expiry
              rezC <- liftIO $ makeJWT clientInfo jwtInfo Nothing
              case rezC of
                Left err -> pure . Left $ "@[authorizeNewSession] makeJWT err: " <> show err
                Right token -> do
                  pure . Right $ AuthenticatedLR (SessionItems clientInfo (decodeUtf8 . toStrict $ token))
-}


authorizeNewSession :: JWTSettings -> LoginForm -> DiffTime -> AIServerApp (Either String LoginResult)
authorizeNewSession jwtInfo lForm sessionDuration = do
  clientInfo <- liftIO fakeClientInfo
  rezC <- liftIO $ makeJWT clientInfo jwtInfo Nothing
  case rezC of
    Left err -> pure . Left $ "@[authorizeNewSession] makeJWT err: " <> show err
    Right token -> do
      pure . Right $ AuthenticatedLR (SessionItems clientInfo (decodeUtf8 . toStrict $ token))
  -- pure $ Left "@[authorizeNewSession] No authorization scheme implemented."

{-
validateUser :: Pool -> BasicAuthData -> IO (AuthResult ClientInfo)
validateUser dbPool (BasicAuthData login pass) = do
  -- TODO: implement proper user access from BasicAuth, for now default to
  --   no session allowed through that auth method.
  pure $ Indefinite
-}
validateUser :: BasicAuthData -> IO (AuthResult ClientInfo)
validateUser (BasicAuthData login pass) = do
  -- TODO: implement proper user access from BasicAuth, for now default to
  --   no session allowed through that auth method.
  pure Indefinite
  
