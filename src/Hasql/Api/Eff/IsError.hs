module Hasql.Api.Eff.IsError where

import Effectful
import GHC.Stack (CallStack)
import Hasql.Api.Eff.Throws

class IsError eff e where
  throwError' :: (eff e :> es) => e -> Eff es a
  catchError' :: Eff (eff e : es) a -> (e -> Eff es a) -> Eff es a
  catchErrorWithCallStack' :: Eff (eff e : es) a -> (CallStack -> e -> Eff es a) -> Eff es a

-- newtype Catch e es a = Catch (CallStack -> e -> Eff es a)

-- instance Functor (Catch e es) where
--   fmap f (Catch handler) = Catch $ \cs e -> f <$> handler cs e

-- instance Applicative (Catch e es) where
--   pure a = Catch $ \_ _ -> pure a
--   (Catch f) <*> (Catch handler) = Catch (\cs e -> f cs e <*> handler cs e)

-- instance Monad (Catch e es) where
--   (Catch handler) >>= f = Catch $ \cs e -> do
--     a <- handler cs e
--     let (Catch h1) = f a
--     h1 cs e

-- catchError :: (e -> Eff es a) -> Catch e es a
-- catchError = Catch . const

-- catchErrorWithCallStack :: (CallStack -> e -> Eff es a) -> Catch e es a
-- catchErrorWithCallStack = Catch

instance IsError Throws e where
  throwError' = throwError
  catchError' = catchError
  catchErrorWithCallStack' = catchErrorWithCallStack
