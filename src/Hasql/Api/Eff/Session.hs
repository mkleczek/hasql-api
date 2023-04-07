{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hasql.Api.Eff.Session (
  Session,
  SessionEffects,
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
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (send)
import Effectful.Error.Static (Error)
import qualified Effectful.Error.Static as E
import Effectful.Reader.Static (runReader)
import qualified Effectful.Reader.Static as E
import Hasql.Api.Eff
import Hasql.Api.Eff.WithResource (WithConnection, withConnection)
import qualified Hasql.Connection as S
import Hasql.Session (CommandError (..), QueryError (..), ResultError (..))
import qualified Hasql.Statement as S

type SessionEffects es = (SqlEff ByteString S.Statement :> es, E.Error QueryError :> es, E.Reader S.Connection :> es, IOE :> es)

type Session a = forall es. (SessionEffects es) => Eff es a

-- instance Functor Session where
--   fmap f (Session eff) = Session (fmap f eff)
--   {-# INLINEABLE fmap #-}

-- instance Applicative Session where
--   pure a = Session (pure a)
--   {-# INLINEABLE pure #-}
--   (Session f) <*> (Session eff) = Session (f <*> eff)
--   {-# INLINEABLE (<*>) #-}

-- instance Monad Session where
--   (Session eff) >>= f =
--     Session $
--       eff >>= \a -> let (Session effb) = f a in effb
--   {-# INLINEABLE (>>=) #-}

instance E.Error QueryError :> es => MonadError QueryError (Eff es) where
  throwError = E.throwError
  {-# INLINEABLE throwError #-}
  catchError eff h = E.catchError eff $ \_ e -> h e -- (Session eff) h = Session $ E.catchError eff $ \_ e -> let (Session eff1) = h e in eff1
  {-# INLINEABLE catchError #-}

instance E.Reader S.Connection :> es => MonadReader S.Connection (Eff es) where
  ask = E.ask
  {-# INLINEABLE ask #-}
  local = E.local
  {-# INLINEABLE local #-}

-- instance MonadIO Session where
--   liftIO ioa = Session $ liftIO ioa
--   {-# INLINEABLE liftIO #-}

{-# INLINEABLE sql #-}
sql :: ByteString -> Session ()
sql q = (send @(SqlEff ByteString S.Statement) $ SqlCommand q)

{-# INLINEABLE statement #-}
statement :: forall parameters result. parameters -> S.Statement parameters result -> Session result
statement params stmt = (send @(SqlEff ByteString S.Statement) $ SqlStatement params stmt)

{-# INLINEABLE runWithHandler #-}
runWithHandler :: SessionEffects es => (Eff es a -> result) -> Session a -> result
runWithHandler h = h

{-# INLINEABLE runWithConnection #-}
runWithConnection :: (WithConnection :> es, Error QueryError :> es, SqlEff ByteString S.Statement :> es, IOE :> es) => Session a -> Eff es a
runWithConnection session = withConnection (run session)

{-# INLINEABLE run #-}
run :: (Error QueryError :> es, SqlEff ByteString S.Statement :> es, IOE :> es) => Session a -> S.Connection -> Eff es a
run session connection = runWithHandler (runReader connection) session
