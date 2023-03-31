module Hasql.Api.Session where

import Data.ByteString (ByteString)
import Hasql.Api
import qualified Hasql.Connection as S
import Hasql.Session as S
import Hasql.Statement (Statement)

instance SimpleSql ByteString S.Session where
  sql = S.sql

instance StatementSql Statement S.Session where
  statement = S.statement

instance RunnableSql S.Session S.Connection S.QueryError where
  run = S.run