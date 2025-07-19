{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Routing.ClientR where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Servant.API.Generic
import Servant.API (JSON, PlainText, ReqBody, Get, Post, Delete, OctetStream
          , (:>), Capture, QueryParam,QueryParam', Optional, Raw, Header, Headers)
import Servant.Auth.Server (Auth, JWT, BasicAuth)

import Api.Types (ClientInfo, Html)
import qualified Api.RequestTypes as Rq
import qualified Api.ResponseTypes as Rr



data PublicRoutes route = PublicRoutes {
    loginPR :: route :- "login" :> ReqBody '[JSON] Rq.LoginForm :> Post '[JSON] Rr.LoginResult
  , helpPR :: route :- "help" :> QueryParam' '[Optional] "needle" Text :> Get '[PlainText] Html
  }
  deriving (Generic)


data PrivateRoutes route = PrivateRoutes {
  client :: route :- "client" :> ToServantApi ClientRoutes
  , repeat :: route :- "repeat" :> ToServantApi RepeatRoutes
  , invoke :: route :- "invoke" :> ToServantApi InvokeRoutes
  , asset :: route :- "asset" :> ToServantApi AssetRoutes
  , retrieve :: route :- "retrieve" :> ToServantApi RetrieveRoutes
  , storage :: route :- "storage" :> ToServantApi StorageRoutes

    -- Admin (JWT only; could add a separate AuthProtect if desired)
  , admin :: route :- "admin" :> ToServantApi AdminRoutes
  }
  deriving (Generic)


data ClientRoutes route = ClientRoutes {
      getClient :: route :- Capture "clientId" UUID :> Get '[JSON] ClientInfo
    , updatePassword :: route :- ReqBody '[JSON] Rq.UpdatePassword :> Post '[JSON] Rr.ReplyClientRequest
    , updatePreferences :: route :- ReqBody '[JSON] Rq.UpdatePreferences :> Post '[JSON] Rr.ReplyClientRequest
    , listServices :: route :- "listServices" :> QueryParam' '[Optional] "filter" Text :> Get '[JSON] [Rr.ClientServiceDescriptor]
  }
  deriving (Generic)

newtype RepeatRoutes route = RepeatRoutes {
  repeatOp :: route :- ReqBody '[JSON] Rq.RepeatRequest :> Post '[JSON] Rr.ReplyClientRequest
  }
  deriving (Generic)

data InvokeRoutes route = InvokeRoutes {
  getResource :: route :- "resource" :> ReqBody '[JSON] Rq.ResourceRequest :> Post '[JSON] Rr.InvokeResponse
  , getResponse :: route :- "response" :> QueryParam "tid" UUID :> QueryParam' '[Optional] "mode" Text :> Get '[JSON] Rr.InvokeResponse
  , invokeService :: route :- ReqBody '[JSON] Rq.InvokeRequest :> Post '[JSON] Rr.InvokeResponse
  }
  deriving (Generic)

newtype AssetRoutes route = AssetRoutes {
  getAsset :: route :-  Capture "assetId" UUID :> QueryParam' '[Optional] "p" Text :> Get '[OctetStream] (Headers '[Header "Content-Type" Text] ByteString)
  }
  deriving (Generic)

newtype RetrieveRoutes route = RetrieveRoutes {
  retrieve :: route :- Capture "requestId" UUID :> Get '[JSON] Rr.InvokeResponse
  }
  deriving (Generic)

data StorageRoutes route = StorageRoutes {
    storagePut :: route :- ReqBody '[JSON] Rq.StoragePut :> Post '[JSON] Rr.ReplyClientRequest
  , storageGet :: route :- Capture "itemId" UUID :> Get '[OctetStream] ByteString
  , storageDel :: route :- Capture "itemId" UUID :> Delete '[JSON] Rr.ReplyClientRequest
  }
  deriving (Generic)

data AdminRoutes route = AdminRoutes {
  svcCreate :: route :- ReqBody '[JSON] Rq.ServiceDescriptor :> Post '[JSON] Rr.ReplyClientRequest
  , svcGet :: route :- Capture "svcId" UUID :> Get '[JSON] Rr.ReplyClientRequest
  , quotaUpdate :: route :- "quota" :> Capture "clientId" UUID
                  :> ReqBody '[JSON] Rq.QuotaUpdate :> Post '[JSON] Rr.ReplyClientRequest
  , newClient :: route :- "newClient" :>ReqBody '[JSON] Rq.ClientCreate :> Post '[JSON] ClientInfo
  }
  deriving (Generic)
