{-# LANGUAGE RankNTypes #-}

module Hasql.Api (SimpleSql (..), StatementSql (..), Sql, RunnableSql (..)) where

class SimpleSql q m where
  sql :: q -> m ()

class StatementSql s m where
  statement :: parameters -> s parameters result -> m result

class (SimpleSql q m, StatementSql s m) => Sql q s m

instance (SimpleSql q m, StatementSql s m) => Sql q s m

class RunnableSql m c e where
  run :: m a -> c -> IO (Either e a)
