module Hasql.Api.Session (
  Sql,
  S.QueryError (..),
  S.ResultError (..),
  S.RowError (..),
  S.CommandError (..),
  runSession,
  runSessionWithConnectionReader,
  runInIO,
) where

import Data.ByteString (ByteString)
import Effectful (Eff, IOE, inject, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Effectful.Reader.Static (Reader, ask)
import Hasql.Api
import Hasql.Api.Eff (SqlEff (..))
import qualified Hasql.Connection as S

import Hasql.Api.Eff.Throws
import qualified Hasql.Session as S
import qualified Hasql.Statement as S

instance SqlQ ByteString S.Session where
  sql = S.sql
instance SqlS S.Statement S.Session where
  statement = S.statement

runSession :: forall es result. (IOE :> es, Throws S.QueryError :> es) => S.Connection -> Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSession connection = interpret $ \env e -> do
  er <- localSeqUnliftIO env $ \_ ->
    S.run
      ( case e of
          SqlCommand q -> S.sql q
          SqlStatement params stmt -> S.statement params stmt
      )
      connection
  throwLeft er

{-# INLINE runSessionWithConnectionReader #-}
runSessionWithConnectionReader :: (IOE :> es, Throws S.QueryError :> es, Reader S.Connection :> es) => Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSessionWithConnectionReader e = ask >>= flip runSession e

runInIO :: S.Connection -> Eff '[SqlEff ByteString S.Statement, Throws S.QueryError, IOE] result -> IO (Either S.QueryError result)
runInIO connection = runEff . toEither . runSession connection

-- flip catchError (pure . Left) $

-- instance RunnableSql (Eff '[SqlEff ByteString S.Statement, IOE]) where
--   type C (Eff '[SqlEff ByteString S.Statement, IOE]) = S.Connection
--   type E (Eff '[SqlEff ByteString S.Statement, IOE]) = S.QueryError
--   run connection = runEff . flip runS connection
