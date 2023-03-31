module Hasql.Api.Eff where

import Data.ByteString (ByteString)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, ask)
import qualified Hasql.Api as HA
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, run, sql, statement)
import Hasql.Statement (Statement)

data ParametrizedSql s :: Effect where
  Statement :: params -> s params result -> ParametrizedSql s m result

data SimpleSql q :: Effect where
  Sql :: q -> SimpleSql q m ()

type instance DispatchOf (ParametrizedSql s) = 'Dynamic
type instance DispatchOf (SimpleSql s) = 'Dynamic

type SqlSession q s es = (ParametrizedSql s :> es, SimpleSql q :> es)

instance SimpleSql q :> es => HA.SimpleSql q (Eff es) where
  sql = send . Sql

instance ParametrizedSql s :> es => HA.StatementSql s (Eff es) where
  statement parms stmt = send $ Statement parms stmt

runSimple :: (IOE :> es, Reader Connection :> es, Error QueryError :> es) => Eff (SimpleSql ByteString : es) a -> Eff es a
runSimple = interpret $ \env (Sql query) -> do
  c <- ask
  res <- localSeqUnliftIO env $ \_ -> do
    Hasql.Session.run (Hasql.Session.sql query) c
  either throwError pure res

runParametrized :: (IOE :> es, Reader Connection :> es, Error QueryError :> es) => Eff (ParametrizedSql Statement : es) a -> Eff es a
runParametrized = interpret $ \env (Statement params stmt) -> do
  c <- ask
  res <- localSeqUnliftIO env $ \_ -> do
    Hasql.Session.run (Hasql.Session.statement params stmt) c
  either throwError pure res