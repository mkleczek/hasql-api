{-# LANGUAGE TypeApplications #-}

module Hasql.Api.Eff (
  Sql (..),
  SqlEff (..),
) where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Hasql.Api (Sql (..))

data SqlEff q (s :: Type -> Type -> Type) :: Effect where
  SqlCommand :: q -> SqlEff q s m ()
  SqlStatement :: params -> s params result -> SqlEff q s m result

type instance DispatchOf (SqlEff q s) = 'Dynamic

instance forall q s es. (SqlEff q s :> es) => Sql q s (Eff es) where
  sql q = send @(SqlEff q s) $ SqlCommand q
  statement params stmt = send @(SqlEff q s) $ SqlStatement params stmt
