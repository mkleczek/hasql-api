module Hasql.Api (Sql (..)) where

import Data.ByteString as B

class Sql s m | m -> s where
  sql :: B.ByteString -> m ()
  statement :: parameters -> s parameters result -> m result

