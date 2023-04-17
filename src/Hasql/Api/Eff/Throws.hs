module Hasql.Api.Eff.Throws (
  Throws,
  throwError,
  catchError,
  catchErrorWithCallStack,
  onError,
  onErrorWithCallStack,
  wrapError,
  wrapErrorWithCallStack,
  toEither,
  throwLeft,
) where

import Control.Exception
import Data.Functor ((<&>))
import Data.Unique
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import GHC.Exception (prettyCallStackLines)
import GHC.Exts (Any)
import GHC.Stack
import Unsafe.Coerce (unsafeCoerce)

data Throws e :: Effect

type instance DispatchOf (Throws e) = 'Static 'NoSideEffects
newtype instance StaticRep (Throws e) = Throws ErrorId

onError :: (Throws e :> es) => Eff es a -> (e -> Eff es ()) -> Eff es a
onError eff = onErrorWithCallStack eff . const

onErrorWithCallStack :: forall es e a. (Throws e :> es) => Eff es a -> (CallStack -> e -> Eff es ()) -> Eff es a
onErrorWithCallStack eff action = unsafeEff $ \es -> do
  Throws eid <- getEnv @(Throws e) es
  catchErrorIO eid (unEff eff es) $ \ew@(ErrorWrapper _ cs e) -> do
    unEff (action cs $ unsafeCoerce e) es
    throwIO ew

-- wrapError :: (Throws e2 :> es) => (e1 -> e2) -> Catch e1 es a
-- wrapError wrap = catchError $ \e -> throwError $ wrap e

-- letsTry :: Eff (Throws e : es) a -> Catch e es a -> Eff es a
-- letsTry eff (Catch handler) = catchErrorWithCallStack eff handler

catchError :: (Show e, IOE :> es) => Eff (Throws e : es) a -> (e -> Eff es a) -> Eff es a
catchError eff handler = catchErrorWithCallStack eff $ \_ e -> do
  liftIO $ putStrLn "catchError handler"
  r <- handler e
  liftIO $ putStrLn "post catchError handler"
  pure r

toEither :: (Show e, IOE :> es) => Eff (Throws e : es) a -> Eff es (Either e a)
toEither eff = (eff <&> Right) `catchError` (pure . Left)

throwLeft :: (Throws e :> es) => Either e a -> Eff es a
throwLeft = either throwError pure

wrapErrorWithCallStack :: (Show e, IOE :> es, Throws wrapper :> es) => Eff (Throws e : es) a -> (CallStack -> e -> wrapper) -> Eff es a
wrapErrorWithCallStack eff wrap = catchErrorWithCallStack eff $ \cs e -> throwError $ wrap cs e

wrapError :: (Show e, IOE :> es, Throws wrapper :> es) => Eff (Throws e : es) a -> (e -> wrapper) -> Eff es a
wrapError eff wrap = catchError eff $ throwError . wrap

-- temp :: Eff '[] ()
-- temp =
--   do
--     x <- throwError "sadfsdfaas"
--     pure x
--     `onError` (\(_ :: String) -> pure ())
--     `wrapError` const @Int @String 5
--     `catchError` (\(_ :: Int) -> throwError "sssss")
--     `catchError` (\(_ :: String) -> pure ())

catchErrorWithCallStack ::
  forall e es a.
  (Show e, IOE :> es) =>
  Eff (Throws e : es) a ->
  (CallStack -> e -> Eff es a) ->
  Eff es a
catchErrorWithCallStack eff handler = unsafeEff $ \es0 -> do
  eid <- newErrorId
  catchErrorIO
    eid
    ( bracket
        (consEnv (Throws @e eid) dummyRelinker es0)
        unconsEnv
        ( \es -> do
            putStrLn "Inside bracket"
            r <- unEff eff es
            putStrLn "No error"
            pure r
        )
    )
    $ \(ErrorWrapper _ cs e) -> do
      putStrLn "caught error"
      print @e $ unsafeCoerce e
      mapM_ putStrLn $ prettyCallStackLines cs
      res <-
        unEff
          ( do
              liftIO $ putStrLn "handler nadler handler handler"
              handler cs $ unsafeCoerce e
          )
          es0
      putStrLn "handled error"
      pure res

-- mask $ \unmask -> do
--   eid <- newErrorId
--   es <- consEnv (Throws @e eid) dummyRelinker es0
--   catchErrorIO
--     eid
--     -- unmask when running eff
--     ( do
--         res <- unmask (unEff eff es) `onException` unconsEnv es
--         unconsEnv es
--         pure res
--     )
--     $ \ew@(ErrorWrapper _ cs e) -> unmask $ do
--       putStrLn "Handling error"
--       print @e $ unsafeCoerce e
--       mapM_ putStrLn $ prettyCallStackLines cs
--       res <- unEff (handler cs $ unsafeCoerce e) es0
--       putStrLn "After handler"
--       pure res

throwError ::
  forall e es a.
  (HasCallStack, Throws e :> es) =>
  -- | The error.
  e ->
  Eff es a
throwError e = unsafeEff $ \es -> do
  Throws eid <- getEnv @(Throws e) es
  throwIO $ ErrorWrapper eid callStack (unsafeCoerce e)

----------------------------------------
-- Helpers

newtype ErrorId = ErrorId Unique
  deriving stock (Eq)

{- | A unique is picked so that distinct 'Error' handlers for the same type
 don't catch each other's exceptions.
-}
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

data ErrorWrapper = ErrorWrapper !ErrorId CallStack Any
instance Show ErrorWrapper where
  showsPrec p (ErrorWrapper _ cs _) =
    ("Effectful.Error.Static.ErrorWrapper\n\n" ++)
      . ("If you see this, most likely there is a stray 'Async' action that\n" ++)
      . ("outlived the scope of the 'Error' effect, was interacted with and threw\n" ++)
      . ("an error to the parent thread. If that scenario sounds unlikely, please\n" ++)
      . ("file a ticket at https://github.com/haskell-effectful/effectful/issues.\n\n" ++)
      . showsPrec p (prettyCallStack cs)
instance Exception ErrorWrapper

catchErrorIO :: ErrorId -> IO a -> (ErrorWrapper -> IO a) -> IO a
catchErrorIO eid m handler = do
  m `catch` \err@(ErrorWrapper etag _ _) -> do
    if eid == etag
      then handler err
      else throwIO err