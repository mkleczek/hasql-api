{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module Hasql.Api (SqlQ (..), SqlS (..), Sql, RunnableSql (..)) where

class SqlQ q m where
  sql :: q -> m ()
class SqlS s m where
  statement :: parameters -> s parameters result -> m result

class (SqlQ q m, SqlS s m) => Sql q s m

instance (SqlQ q m, SqlS s m) => Sql q s m

class RunnableSql m where
  type C m
  type E m
  run :: m a -> C m -> IO (Either (E m) a)

-- type Session a = forall m. (Sql m) => m a

-- type Session :: Type -> Type
