module Hasql.Api (Sql (..), RunnableSql (..)) where

import Data.ByteString as B

class Sql s m where
  sql :: B.ByteString -> m ()
  statement :: parameters -> s parameters result -> m result

class RunnableSql m c e where
  run :: m a -> c -> IO (Either e a)
