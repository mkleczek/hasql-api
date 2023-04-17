module Hasql.Api.Eff.Util where

import Effectful
import Effectful.Dispatch.Dynamic
import Hasql.Api.Eff (SqlEff (..))

logSql :: forall q s es a. (Show q, (forall params result. Show (s params result)), IOE :> es) => Eff (SqlEff q s : es) a -> Eff (SqlEff q s : es) a
logSql = interpose @(SqlEff q s) $ \_ -> \case
  SqlCommand query -> do
    liftIO $ print query
    send @(SqlEff q s) $ SqlCommand query
  SqlStatement params statement -> do
    liftIO $ print statement
    send @(SqlEff q s) $ SqlStatement params statement
