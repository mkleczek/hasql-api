module Hasql.Api.Eff.Session.Run (
  Session (..),
  run,
  QueryError (..),
  ResultError (..),
  CommandError (..),
  RowError (..),
) where

import qualified Hasql.Connection as S
import Hasql.Session (CommandError (..), QueryError (..), ResultError (..), RowError (..))

newtype Session a = Session (S.Connection -> IO (Either QueryError a))

{-# INLINE run #-}
run :: Session a -> S.Connection -> IO (Either QueryError a)
run (Session runner) = runner
