module Hasql.Api.Session where

import Hasql.Session as S
import Hasql.Statement (Statement)

instance Sql Statement S.Session where
  sql = S.sql
  statement = S.statement
