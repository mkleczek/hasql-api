{-# LANGUAGE TypeApplications #-}

module Hasql.Api.Eff.Session (
  Session,
  sql,
  statement,
  runSession,
  QueryError (..),
  ResultError (..),
  CommandError (..),
) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Data.ByteString (ByteString)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (send)
import qualified Effectful.Error.Static as E
import Effectful.Reader.Static (runReader)
import qualified Effectful.Reader.Static as E
import Hasql.Api.Eff
import Hasql.Api.Session (runInIO)
import qualified Hasql.Connection as S
import Hasql.Session (CommandError (..), QueryError (..), ResultError (..))
import qualified Hasql.Statement as S

newtype Session a = Session (forall es. (SqlEff ByteString S.Statement :> es, E.Error QueryError :> es, E.Reader S.Connection :> es, IOE :> es) => Eff es a)
instance Functor Session where
  fmap f (Session eff) = Session (fmap f eff)

instance Applicative Session where
  pure a = Session (pure a)
  (Session f) <*> (Session eff) = Session (f <*> eff)

instance Monad Session where
  (Session eff) >>= f =
    Session $
      eff >>= \a -> let (Session effb) = f a in effb

instance MonadError QueryError Session where
  throwError e = Session $ E.throwError e
  catchError (Session eff) h = Session $ E.catchError eff $ \_ e -> let (Session eff1) = h e in eff1

instance MonadReader S.Connection Session where
  ask = Session E.ask
  local f (Session eff) = Session $ E.local f eff

instance MonadIO Session where
  liftIO ioa = Session $ liftIO ioa

sql :: ByteString -> Session ()
sql q = Session (send @(SqlEff ByteString S.Statement) $ SqlCommand q)

statement :: forall parameters result. parameters -> S.Statement parameters result -> Session result
statement params stmt = Session (send @(SqlEff ByteString S.Statement) $ SqlStatement params stmt)

runSession :: Session a -> S.Connection -> IO (Either QueryError a)
runSession (Session eff) c = runInIO c $ runReader c eff
