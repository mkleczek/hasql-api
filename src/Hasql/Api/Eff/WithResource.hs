module Hasql.Api.Eff.WithResource (
  WithResource (..),
  WithConnection,
  withResource,
  Connection,
  withConnection,
  nonPooledConnection,
) where

import Control.Exception.Safe (bracket)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, localSeqUnliftIO, send)
import Effectful.Error.Static (Error, throwError)
import Hasql.Connection (Connection, ConnectionError, Settings, acquire, release)

data WithResource r :: Effect where
  WithResource :: (r -> m a) -> WithResource r m a

type instance DispatchOf (WithResource r) = 'Dynamic

type WithConnection = WithResource Connection

withResource :: (WithResource r :> es) => (r -> Eff es a) -> Eff es a
withResource = send . WithResource

withConnection :: (WithConnection :> es) => (Connection -> Eff es a) -> Eff es a
withConnection = withResource

nonPooledConnection :: (Error ConnectionError :> es, IOE :> es) => Settings -> Eff (WithConnection : es) a -> Eff es a
nonPooledConnection connectionSettings = interpret $ \env (WithResource action) -> do
  bracket
    ( do
        connectResult <- localSeqUnliftIO env $ \_ -> acquire connectionSettings
        either throwError pure connectResult
    )
    (\c -> localSeqUnliftIO env $ \_ -> release c)
    (\c -> localSeqUnlift env $ \unlift -> unlift $ action c)
