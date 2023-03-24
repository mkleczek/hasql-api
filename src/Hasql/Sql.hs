{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.ByteString qualified as B

class Sql s m where
  sql :: B.ByteString -> m ()
  statement :: parameters -> s parameters result -> m result
