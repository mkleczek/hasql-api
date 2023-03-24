module Hasql.Api (Sql) where

import Data.ByteString as B
import Hasql.Session as S
import Hasql.Statement (Statement)

class Sql s m where
  sql :: B.ByteString -> m ()
  statement :: parameters -> s parameters result -> m result

instance Sql Statement S.Session where
  sql = S.sql
  statement = S.statement
