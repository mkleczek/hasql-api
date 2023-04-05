{-# LANGUAGE LambdaCase #-}

module Hasql.Api.Eff.WithResource (
  WithResource (..),
  WithConnection,
  DynamicResource (..),
  DynamicConnection,
  withResource,
  Connection,
  withConnection,
  nonPooledConnection,
  acquire,
  release,
  runWithDynamic,
  runConnecting,
) where

import Control.Exception.Safe (bracket)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, localSeqUnliftIO, reinterpret, send)
import Effectful.Error.Static (Error, throwError)
import Hasql.Connection (Connection, ConnectionError, Settings)
import qualified Hasql.Connection as C

data WithResource r :: Effect where
  WithResource :: (r -> m a) -> WithResource r m a

type instance DispatchOf (WithResource r) = 'Dynamic

type WithConnection = WithResource Connection

data DynamicResource resource :: Effect where
  Acquire :: DynamicResource resource m resource
  Release :: resource -> DynamicResource resource m ()

type instance DispatchOf (DynamicResource r) = 'Dynamic

withResource :: (WithResource r :> es) => (r -> Eff es a) -> Eff es a
withResource = send . WithResource

withConnection :: (WithConnection :> es) => (Connection -> Eff es a) -> Eff es a
withConnection = withResource

-- nonPooledConnection :: (Error ConnectionError :> es, IOE :> es) => Settings -> Eff (WithConnection : es) a -> Eff es a
-- nonPooledConnection connectionSettings = interpret $ \env (WithResource action) -> do
--   bracket
--     ( do
--         connectResult <- localSeqUnliftIO env $ \_ -> C.acquire connectionSettings
--         either throwError pure connectResult
--     )
--     (\c -> localSeqUnliftIO env $ \_ -> C.release c)
--     (\c -> localSeqUnlift env $ \unlift -> unlift $ action c)

type DynamicConnection = DynamicResource Connection

acquire :: (DynamicResource r :> es) => Eff es r
acquire = send Acquire

release :: forall r es. (DynamicResource r :> es) => r -> Eff es ()
release = send . Release

runWithDynamic :: forall r es a. (DynamicResource r :> es) => Eff (WithResource r : es) a -> Eff es a
runWithDynamic = interpret $ \env (WithResource action) ->
  bracket
    acquire
    release
    $ \res -> localSeqUnlift env $ \unlift -> unlift $ action res

runConnecting :: (Error ConnectionError :> es, IOE :> es) => Settings -> Eff (DynamicConnection : es) a -> Eff es a
runConnecting settings = interpret $ \env -> \case
  Acquire -> localSeqUnliftIO env (const $ C.acquire settings) >>= either throwError pure
  (Release connection) -> localSeqUnliftIO env $ const $ C.release connection

nonPooledConnection :: (Error ConnectionError :> es, IOE :> es) => Settings -> Eff (WithConnection : DynamicConnection : es) a -> Eff es a
nonPooledConnection s = runConnecting s . runWithDynamic
