module Hasql.Api.Eff.Session.Legacy (
  module Hasql.Api.Eff.Session,
  run,
) where

import Effectful.Reader.Static (runReader)
import Hasql.Api.Eff.Session hiding (run)
import Hasql.Api.Session (runInIO)
import qualified Hasql.Connection as S

run :: Session a -> S.Connection -> IO (Either QueryError a)
run session connection = runWithHandler handler session
  where
    handler eff = runInIO connection $ runReader connection eff
