{-# LANGUAGE QuasiQuotes #-}

module Service.DbOps where

import Service.Types

import Data.Int (Int32)
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Vector as Vc
import Data.UUID (UUID)

import qualified Hasql.TH as TH
import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)


{-
create table if not exists aiservice (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , name text not null
  , description text not null
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);


create table if not exists servfunction (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , label text not null
  , aiservice_fk int not null references aiservice(uid)
  , pricingRule text not null
  , privilegeRule text not null
  , endpoint text not null
  , category varchar(32)[] not null
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);

create table if not exists servaccount (
  uid serial primary key
  , service_fk int not null references aiservice(uid)
  , acctName varchar(255) not null
  , authMethod int not null default 1     -- 1: apiKey, 2: oauth2.
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);

create table if not exists servAuth (
  servaccount_fk int not null references servaccount(uid)
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
  , idlabel text not null
  , secret text not null
  , created_at timestamp not null default now()
);

-}


getServiceDefs :: Pool -> IO (Either String (Mp.Map Text TopDescription))
getServiceDefs pgPool = do
  eiRezA <- use pgPool $
     statement () [TH.vectorStatement|
      select
        uid::int4, eid::uuid, name::text, description::text
      from aiservice
      where status = 1
    |]
  case eiRezA of
    Right services -> do
      let
        uids = Vc.map (\(uid, _, _, _) -> uid) services
      eiRezB <- use pgPool $
         statement uids [TH.vectorStatement|
          select
            uid::int4, eid::uuid, aiservice_fk::int4, label::text, pricingRule::text, privilegeRule::text, endpoint::text, category::text[]
          from servfunction
          where status = 1 and aiservice_fk = any($1::int4[])
          order by uid asc
        |]
      case eiRezB of
        Right functions -> do
          let
            functionList = Vc.map toFunctionDescription functions
            functionMap = Vc.foldl' (\accum (service_fk, aFct) ->
                    Mp.insertWith (<>) service_fk (Vc.singleton aFct) accum
                ) (Mp.empty :: Mp.Map Int32 (Vc.Vector FunctionDescription)) functionList
            serviceDefs = Mp.fromList . Vc.toList $ Vc.map (toTopDescription functionMap) services
          pure $ Right serviceDefs
        Left err -> do
          pure . Left $ "@[getServiceDefs] servFunction err: " <> show err
    Left err -> do
      pure . Left $ "@[getServiceDefs] topDescription err: " <> show err
  where
    toTopDescription :: Mp.Map Int32 (Vc.Vector FunctionDescription) -> (Int32, UUID, Text, Text) -> (Text, TopDescription)
    toTopDescription functionMap (uid, eid, name, description) =
        (name, TopDescription uid eid name description (Mp.findWithDefault Vc.empty uid functionMap))
    toFunctionDescription :: (Int32, UUID, Int32, Text, Text, Text, Text, Vc.Vector Text) -> (Int32, FunctionDescription)
    toFunctionDescription (uid, eid, service_fk, label, pricingRule, privilegeRule, endpoint, category) = 
        (service_fk, FunctionDescription uid eid label pricingRule privilegeRule endpoint category)


getServiceAccess :: Pool -> Int32 -> Text -> IO (Either String (Maybe (Text, Text)))
getServiceAccess pgPool servID acctName = do
  eiRez <- use pgPool $
     statement (servID, acctName) [TH.vectorStatement|
      select
        c.idlabel::text, c.secret::text
      from aiservice a 
      join servaccount b on a.uid = b.service_fk
      join servauth c on b.uid = c.servaccount_fk
      where 
        a.uid = $1::int4
        and b.acctName = $2::text and b.status = 1
      order by c.created_at desc
      limit 1
    |]
  case eiRez of
    Right auths -> do
      pure . Right $ if Vc.null auths then Nothing else Just (Vc.head auths)
    Left err -> do
      pure . Left $ "@[getServiceAccess] err: " <> show err

