{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hasql.Api.Eff.Session (
  Session,
  SessionEffects,
  toEff,
  sql,
  statement,
  QueryError (..),
  ResultError (..),
  CommandError (..),
  runWithHandler,
  runWithConnection,
  run,
) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Data.ByteString (ByteString)
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Reader.Static (runReader)
import qualified Effectful.Reader.Static as E
import Hasql.Api.Eff
import Hasql.Api.Eff.Throws
import qualified Hasql.Api.Eff.Throws as T
import Hasql.Api.Eff.WithResource (WithConnection, withConnection)
import qualified Hasql.Connection as S
import Hasql.Session (CommandError (..), QueryError (..), ResultError (..))
import qualified Hasql.Statement as S

type SessionEffects es = (SqlEff ByteString S.Statement :> es, Throws QueryError :> es, E.Reader S.Connection :> es, IOE :> es)

newtype Session a = Session (forall es. (SessionEffects es) => Eff es a)
instance Functor Session where
  fmap f (Session eff) = Session (fmap f eff)
  {-# INLINEABLE fmap #-}

instance Applicative Session where
  pure a = Session (pure a)
  {-# INLINEABLE pure #-}
  (Session f) <*> (Session eff) = Session (f <*> eff)
  {-# INLINEABLE (<*>) #-}

instance Monad Session where
  (Session eff) >>= f =
    Session $
      eff >>= \a -> let (Session effb) = f a in effb
  {-# INLINEABLE (>>=) #-}

instance MonadError QueryError Session where
  throwError e = Session $ T.throwError e
  {-# INLINEABLE throwError #-}
  catchError (Session eff) handler = Session $ inject $ T.catchError eff $ \e -> do
    let (Session eff1) = handler0 e handler
    eff1
    where
      handler0 err h = do
        liftIO $ putStrLn "MondaError"
        h err
  {-# INLINEABLE catchError #-}

instance MonadReader S.Connection Session where
  ask = Session E.ask
  {-# INLINEABLE ask #-}
  local f (Session eff) = Session $ E.local f eff
  {-# INLINEABLE local #-}
instance MonadIO Session where
  liftIO ioa = Session $ liftIO ioa
  {-# INLINEABLE liftIO #-}

{-# INLINEABLE sql #-}
sql :: ByteString -> Session ()
sql q = Session (send @(SqlEff ByteString S.Statement) $ SqlCommand q)

{-# INLINEABLE statement #-}
statement :: forall parameters result. parameters -> S.Statement parameters result -> Session result
statement params stmt = Session (send @(SqlEff ByteString S.Statement) $ SqlStatement params stmt)

{-# INLINEABLE runWithHandler #-}
runWithHandler :: SessionEffects es => (Eff es a -> result) -> Session a -> result
runWithHandler h (Session eff) = h eff

{-# INLINEABLE runWithConnection #-}
runWithConnection :: forall es a. (Throws QueryError :> es, SqlEff ByteString S.Statement :> es, IOE :> es, WithConnection (Eff es) :> es) => Session a -> Eff es a
runWithConnection session = withConnection (run @es session)

{-# INLINEABLE run #-}
run :: (Throws QueryError :> es, SqlEff ByteString S.Statement :> es, IOE :> es) => Session a -> S.Connection -> Eff es a
run session connection = runWithHandler (runReader connection) session

{-# INLINE toEff #-}
toEff :: SessionEffects es => Session a -> Eff es a
toEff (Session eff) = eff