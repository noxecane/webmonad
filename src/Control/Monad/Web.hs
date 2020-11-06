{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.X.Web where

import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.X.AlaCarte (inject, (:+:), (:<:))
import Database.PostgreSQL.Simple (Connection)
import Opaleye.X
  ( DbError,
    InsertError,
    MonadDB (..),
    safeInsertOne,
    safeInsertOneReturningId,
    selectOne_,
    select_,
  )

newtype WebConfig = WebConfig
  { getDBConn :: Connection
  }

type WebError = DbError :+: InsertError

newtype WebM es a = WebM {getWebM :: ExceptT es (ReaderT WebConfig IO) a}
  deriving (Applicative, Functor, Monad, MonadReader WebConfig, MonadError es, MonadIO)

runWebM :: WebConfig -> WebM es a -> IO (Either es a)
runWebM config = flip runReaderT config . runExceptT . getWebM

throwCoError :: (MonadError es m, e :<: es) => e -> m a
throwCoError = throwError . inject

liftEither :: (MonadError es m, e :<: es) => Either e a -> m a
liftEither = either throwCoError return

instance (InsertError :<: es) => MonadDB (WebM es) where
  insertOne t h = do
    conn <- asks getDBConn
    result <- liftIO $ safeInsertOne conn t h
    liftEither result

  insertOneReturningId t h rId = do
    conn <- asks getDBConn
    result <- liftIO $ safeInsertOneReturningId conn t h rId
    liftEither result

  selectOne sel = asks getDBConn >>= liftIO . flip selectOne_ sel

  select sel = asks getDBConn >>= liftIO . flip select_ sel
