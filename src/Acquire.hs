{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Acquire
  ( Acquire
  , Cleaner
  , CleanerT (..)
  , MonadCleaner (..)
  , mkAcquire
  , mkCleaner
  , fromAcquire
  , acquire
  , cleanup
  , acquireT
  , register
  , runCleanerT
  , collect
  ) where

import Prelude hiding (reverse)

import Data.Tuple (swap)
import Data.Sequence (Seq (..), (><), singleton, reverse)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)

data Acquire a = Acquire (IO a) (a -> IO ())

newtype Cleaner = Cleaner { unCleaner :: Seq Clean }

instance Semigroup Cleaner where
  (<>) = (Cleaner .) . (. unCleaner) . (><) . unCleaner

instance Monoid Cleaner where
  mempty = Cleaner mempty

data Clean = forall a . Clean (a -> IO ()) a

newtype CleanerT m a = CleanerT { unCleanerT :: WriterT Cleaner m a } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class MonadIO m => MonadCleaner m where
  liftCleanerT :: CleanerT IO a -> m a

instance MonadCleaner (CleanerT IO) where
  liftCleanerT ioa = CleanerT $ unCleanerT ioa

runCleanerT :: MonadIO m => CleanerT m a -> m a
runCleanerT ct = do
  (a, w) <- runWriterT . unCleanerT $ ct
  liftIO . cleanup $ w
  pure a

collect :: Monad m => CleanerT m a -> m (Cleaner, a)
collect = (swap <$>) . runWriterT . unCleanerT

register :: (MonadCleaner m) => Cleaner -> m ()
register = liftCleanerT . CleanerT . tell

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire = Acquire

mkCleaner :: IO () -> Cleaner
mkCleaner = Cleaner . singleton . flip Clean () . const

fromAcquire :: MonadIO m => Acquire a -> m (Cleaner, a)
fromAcquire (Acquire acq cleaner) = acquire acq cleaner

acquire :: MonadIO m => IO a -> (a -> IO ()) -> m (Cleaner, a)
acquire acq cleaner = do
  res <- liftIO acq
  pure (Cleaner . singleton . Clean cleaner $ res, res)

cleanup :: Cleaner -> IO ()
cleanup = foldMap clean . reverse . unCleaner
  where
    clean :: Clean -> IO ()
    clean (Clean cleaner res) = cleaner res

acquireT :: MonadCleaner m => IO r -> (r -> IO ()) -> m r
acquireT create destroy = do
  res <- liftIO create
  register . mkCleaner . destroy $ res
  pure res

test :: IO Int
test = runCleanerT $ do
  register $ mkCleaner $ print "1"
  register $ mkCleaner $ print "2"
  register $ mkCleaner $ print "3"
  pure 0
