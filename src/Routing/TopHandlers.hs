{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Routing.TopHandlers where

import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.API.Generic (ToServant)

import Api.Types (AIServerApp)
import qualified Routing.ClientH as Ch
import Routing.TopDefs


serverApiT :: ToServant TopRoutes (AsServerT AIServerApp)
serverApiT =
  genericServerT $ TopRoutes {
    anonymous = Ch.publicHandlers
    , authenticated = Ch.privateHandlers
  }

