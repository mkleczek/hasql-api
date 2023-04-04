{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module Hasql.Api (Sql (..), RunnableSql (..)) where

class Sql q s m where
  sql :: q -> m ()
  statement :: parameters -> s parameters result -> m result

class RunnableSql m where
  type C m
  type E m
  run :: m a -> C m -> IO (Either (E m) a)

-- type Session a = forall m. (Sql m) => m a

-- type Session :: Type -> Type
