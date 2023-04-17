module Hasql.Api.Eff.Session.Legacy (
  module Hasql.Api.Eff.Session,
  run,
) where

import Effectful.Reader.Static (runReader)
import Hasql.Api.Eff.Session hiding (run)
import Hasql.Api.Eff.Util
import Hasql.Api.Session (runInIO)
import qualified Hasql.Connection as S
import Hasql.Statement (Statement (Statement))

instance Show (Statement p r) where
  show (Statement s _ _ _) = show s

run :: Session a -> S.Connection -> IO (Either QueryError a)
run session connection = runWithHandler handler session
  where
    handler eff = runInIO connection $ logSql $ runReader connection eff
