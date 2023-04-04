module Hasql.Api.Session (
  RunnableSql (..),
  Sql,
  S.QueryError (..),
  S.ResultError (..),
  S.RowError (..),
  S.CommandError (..),
  runInIO,
) where

import Data.ByteString (ByteString)
import Effectful (Eff, IOE, MonadIO (liftIO), inject, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Reader.Static (Reader, ask)
import Hasql.Api
import Hasql.Api.Eff (SqlEff (..))
import Hasql.Connection (Connection)
import qualified Hasql.Connection as S

import qualified Hasql.Session as S
import qualified Hasql.Statement as S

instance SqlQ ByteString S.Session where
  sql = S.sql
instance SqlS S.Statement S.Session where
  statement = S.statement

instance RunnableSql S.Session where
  type C S.Session = S.Connection
  type E S.Session = S.QueryError
  run = S.run

runSession :: forall es result. (IOE :> es, Error S.QueryError :> es) => Connection -> Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSession connection = interpret $ \env e -> do
  er <- localSeqUnliftIO env $ \_ -> do
    putStrLn "Jestem tutaj api eff"
    result <-
      let x = case e of
            SqlCommand q -> do
              liftIO $ putStrLn $ "query: " ++ show q
              S.sql q
            SqlStatement params stmt -> do
              liftIO $ putStrLn "statement"
              S.statement params stmt
       in do
            putStrLn "Jestem tutaj after 111"
            res <- S.run x connection
            pure res
    putStrLn "Jestem tutaj after"
    pure result
  either throwError pure er

runSessionWithConnectionReader :: (IOE :> es, Error S.QueryError :> es, Reader Connection :> es) => Eff (SqlEff ByteString S.Statement : es) result -> Eff es result
runSessionWithConnectionReader e = ask >>= flip runSession e

runS :: (IOE :> es) => Connection -> Eff (Error S.QueryError : SqlEff ByteString S.Statement : es) result -> Eff es (Either S.QueryError result)
runS connection eff = runErrorNoCallStack (runSession connection (inject eff))

runInIO :: S.Connection -> Eff '[Error S.QueryError, SqlEff ByteString S.Statement, IOE] result -> IO (Either S.QueryError result)
runInIO connection eff = runEff $ runS connection eff

-- instance RunnableSql (Eff '[SqlEff ByteString S.Statement, IOE]) where
--   type C (Eff '[SqlEff ByteString S.Statement, IOE]) = S.Connection
--   type E (Eff '[SqlEff ByteString S.Statement, IOE]) = S.QueryError
--   run connection = runEff . flip runS connection
