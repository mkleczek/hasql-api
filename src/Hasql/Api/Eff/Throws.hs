module Hasql.Api.Eff.Throws (
  Throws,
  throwError,
  catchError,
  catchError',
  -- catchErrorWithCallStack,
  -- onError,
  -- onErrorWithCallStack,
  -- wrapError,
  -- wrapErrorWithCallStack,
  toEither,
  throwLeft,
) where

import Control.Exception
import Data.Unique
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import GHC.Stack
import Type.Reflection (Typeable)
import Unsafe.Coerce (unsafeCoerce)

data Throws e :: Effect

type instance DispatchOf (Throws e) = 'Static 'NoSideEffects
newtype instance StaticRep (Throws e) = Throws ErrorId

runError ::
  forall e es a.
  Typeable e =>
  Eff (Throws e : es) a ->
  Eff es (Either (CallStack, e) a)
runError m = unsafeEff $ \es0 -> do
  eid <- newErrorId
  result <-
    try $
      bracket
        (consEnv (Throws @e eid) dummyRelinker es0)
        unconsEnv
        (unEff m)
  case result of
    Right a -> pure $ Right a
    Left ex -> tryHandler ex eid (curry Left) $ throwIO ex

-- | Handle errors of type @e@. In case of an error discard the 'CallStack'.
runErrorNoCallStack ::
  forall e es a.
  Typeable e =>
  Eff (Throws e : es) a ->
  Eff es (Either e a)
runErrorNoCallStack = fmap (either (Left . snd) Right) . runError

catchError' :: forall e es a. Typeable e => Eff (Throws e : es) a -> (e -> Eff es a) -> Eff es a
catchError' eff handler = unsafeEff $ \es0 -> do
  eid <- newErrorId
  catchErrorIO
    eid
    ( bracket
        (consEnv (Throws @e eid) dummyRelinker es0)
        unconsEnv
        (unEff eff)
    )
    (const (flip unEff es0 . handler))

-- | Throw an error of type @e@.
throwError ::
  forall e es a.
  (HasCallStack, Typeable e, Throws e :> es) =>
  -- | The error.
  e ->
  Eff es a
throwError e = unsafeEff $ \es -> do
  Throws eid <- getEnv @(Throws e) es
  throwIO @(ErrorWrapper e) $ ErrorWrapper eid callStack (unsafeCoerce e)

-- | Handle an error of type @e@.
catchError ::
  forall e es a.
  (Typeable e, Throws e :> es) =>
  -- | The inner computation.
  Eff es a ->
  -- | A handler for errors in the inner computation.
  (CallStack -> e -> Eff es a) ->
  Eff es a
catchError m handler = unsafeEff $ \es -> do
  Throws eid <- getEnv @(Throws e) es
  catchErrorIO eid (unEff m es) $ \cs e -> do
    unEff (handler cs e) es

{- | The same as @'flip' 'catchError'@, which is useful in situations where the
 code for the handler is shorter.
-}
handleError ::
  forall e es a.
  (Typeable e, Throws e :> es) =>
  -- | A handler for errors in the inner computation.
  (CallStack -> e -> Eff es a) ->
  -- | The inner computation.
  Eff es a ->
  Eff es a
handleError = flip catchError

{- | Similar to 'catchError', but returns an 'Either' result which is a 'Right'
 if no error was thrown and a 'Left' otherwise.
-}
tryError ::
  forall e es a.
  (Typeable e, Throws e :> es) =>
  -- | The inner computation.
  Eff es a ->
  Eff es (Either (CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)

----------------------------------------
-- Helpers

newtype ErrorId = ErrorId Unique
  deriving stock (Eq)

{- | A unique is picked so that distinct 'Error' handlers for the same type
 don't catch each other's exceptions.
-}
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

tryHandler ::
  forall e r.
  Typeable e =>
  SomeException ->
  ErrorId ->
  (CallStack -> e -> r) ->
  IO r ->
  IO r
tryHandler ex eid0 handler next = case fromException @(ErrorWrapper e) ex of
  Just (ErrorWrapper eid cs e)
    | eid0 == eid -> pure $ handler cs (unsafeCoerce e)
    | otherwise -> next
  Nothing -> next

data ErrorWrapper e = ErrorWrapper !ErrorId CallStack e
instance Show (ErrorWrapper e) where
  showsPrec p (ErrorWrapper _ cs _) =
    ("Effectful.Error.Static.ErrorWrapper\n\n" ++)
      . ("If you see this, most likely there is a stray 'Async' action that\n" ++)
      . ("outlived the scope of the 'Error' effect, was interacted with and threw\n" ++)
      . ("an error to the parent thread. If that scenario sounds unlikely, please\n" ++)
      . ("file a ticket at https://github.com/haskell-effectful/effectful/issues.\n\n" ++)
      . showsPrec p (prettyCallStack cs)
instance Typeable e => Exception (ErrorWrapper e)

catchErrorIO :: forall e a. (Typeable e) => ErrorId -> IO a -> (CallStack -> e -> IO a) -> IO a
catchErrorIO eid m handler = do
  catch @(ErrorWrapper e) m $ \err@(ErrorWrapper etag cs e) -> do
    if eid == etag
      then handler cs e
      else throwIO err

toEither :: Typeable e => Eff (Throws e : es) a -> Eff es (Either e a)
toEither = runErrorNoCallStack

throwLeft :: (Typeable e, Throws e :> es) => Either e a -> Eff es a
throwLeft = either throwError pure