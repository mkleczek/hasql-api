module Hasql.Api.Session (..) where

import Hasql.Api
import qualified Hasql.Connection as S
import Hasql.Session as S
import Hasql.Statement (Statement)

instance Sql Statement S.Session where
  sql = S.sql
  statement = S.statement

instance RunnableSql S.Session S.Connection S.QueryError where
  run = S.run