module Hasql.Api.Eff.WithResource (
  WithResource (..),
  WithConnection,
  DynamicResource (..),
  DynamicConnection,
  withResource,
  Connection,
  withConnection,
  acquire,
  release,
  runWithDynamic,
  runConnecting,
  runReaderWithResource,
) where

import Control.Exception.Safe (bracket)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, Subset, inject, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnliftIO, send)
import qualified Effectful.Error.Static as E
import Hasql.Connection (Connection, ConnectionError, Settings)
import qualified Hasql.Connection as C

import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (Reader, runReader)

data WithResource r eff :: Effect where
  WithResource :: (r -> eff a) -> WithResource r eff m a

type instance DispatchOf (WithResource r eff) = 'Dynamic

type WithConnection eff = WithResource Connection eff

data DynamicResource resource :: Effect where
  Acquire :: DynamicResource resource m resource
  Release :: resource -> DynamicResource resource m ()

type instance DispatchOf (DynamicResource r) = 'Dynamic

withResource :: forall r eff es a. (WithResource r eff :> es) => (r -> eff a) -> Eff es a
withResource = send . WithResource

withConnection :: (WithConnection eff :> es) => (Connection -> eff a) -> Eff es a
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

type ActionEffHandler effc es = forall a les. effc les => Eff les a -> Eff es a

runWithDynamicH :: (DynamicResource r :> es, Subset les es) => (forall b. Eff les b -> Eff es b) -> Eff (WithResource r (Eff les) : es) a -> Eff es a
runWithDynamicH handler = interpret $ \_ (WithResource action) ->
  bracket
    acquire
    release
    (handler . action)

runWithDynamic :: (DynamicResource r :> es, Subset les es) => Eff (WithResource r (Eff les) : es) a -> Eff es a
runWithDynamic = runWithDynamicH inject

runConnecting :: (E.Error ConnectionError :> es, IOE :> es) => Settings -> Eff (DynamicConnection : es) a -> Eff es a
runConnecting settings = interpret $ \env -> \case
  Acquire -> localSeqUnliftIO env (const $ C.acquire settings) >>= either throwError pure
  Release connection -> localSeqUnliftIO env $ const $ C.release connection

-- nonPooledConnection :: forall es les a. (E.Error ConnectionError :> es, IOE :> es, Subset les es) => Settings -> Eff (WithConnection (Eff les) : es) a -> Eff es a
-- nonPooledConnection settings eff = runConnecting settings $ runWithDynamic (prepend eff)

-- prepend :: Eff (e1 : es) a -> Eff (e1 : e2 : es) a
-- prepend = inject

runReaderWithResource :: (WithResource r (Eff les) :> es) => Eff (Reader r : les) a -> Eff es a
runReaderWithResource eff = withResource $ flip runReader eff
