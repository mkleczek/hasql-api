module Hasql.Api.Session (
  Session,
  SimpleSql (..),
  StatementSql (..),
  RunnableSql (..),
  Sql,
  S.QueryError (..),
  S.ResultError (..),
  S.RowError (..),
  S.CommandError (..),
) where

import Data.ByteString (ByteString)
import Effectful (Eff, IOE, inject, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, ask)
import Hasql.Api
import Hasql.Api.Eff (SqlEff (..))
import Hasql.Connection (Connection)
import qualified Hasql.Connection as S

import qualified Hasql.Session as S
import qualified Hasql.Statement as S

instance SimpleSql ByteString S.Session where
  sql = S.sql

instance StatementSql S.Statement S.Session where
  statement = S.statement

instance RunnableSql S.Session S.Connection S.QueryError where
  run = S.run

type Session a = forall m. (Monad m, SimpleSql ByteString m, StatementSql S.Statement m) => m a

runSession :: forall es result. (IOE :> es, Error S.QueryError :> es) => Connection -> Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSession connection = interpret $ \env e -> do
  er <- localSeqUnliftIO env $ \_ ->
    S.run
      ( case e of
          SqlCommand q -> S.sql q
          SqlStatement params stmt -> S.statement params stmt
      )
      connection
  either throwError pure er

runSessionWithConnectionReader :: (IOE :> es, Error S.QueryError :> es, Reader Connection :> es) => Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSessionWithConnectionReader e = ask >>= flip runSession e

runS :: (IOE :> es) => Connection -> Eff (SqlEff ByteString S.Statement : es) result -> Eff es (Either S.QueryError result)
runS connection eff = runErrorNoCallStack (runSession connection (inject eff))

instance RunnableSql (Eff '[SqlEff ByteString S.Statement, IOE]) S.Connection S.QueryError where
  run connection = runEff . flip runS connection