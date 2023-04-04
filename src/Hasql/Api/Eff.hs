{-# LANGUAGE TypeApplications #-}

module Hasql.Api.Eff (
  SqlEff (..),
) where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Hasql.Api (SqlQ (sql), SqlS (statement))

data SqlEff q (s :: Type -> Type -> Type) :: Effect where
  SqlCommand :: q -> SqlEff q s m ()
  SqlStatement :: params -> s params result -> SqlEff q s m result

type instance DispatchOf (SqlEff q s) = 'Dynamic

instance (SqlEff q s :> es) => SqlQ q (Eff es) where
  sql q = send @(SqlEff q s) $ SqlCommand q

instance (SqlEff q s :> es) => SqlS s (Eff es) where
  statement params stmt = send @(SqlEff q s) $ SqlStatement params stmt
