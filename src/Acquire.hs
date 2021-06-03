{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}

module Acquire
  ( Acquire
  , Cleaner
  , mkAcquire
  , mkCleaner
  , fromAcquire
  , acquire
  , cleanup
  ) where

import Prelude hiding (reverse)

import Data.Bifunctor (bimap, first, second)
import Data.Sequence (Seq (..), (><), singleton, reverse)

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Carrier.Writer.Strict (runWriter)
-- import Control.Effect.Writer (Has, Writer (..), tell)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Unlift --(MonadUnliftIO (..))

data Acquire a = Acquire (IO a) (a -> IO ())

newtype Cleaner = Cleaner { unCleaner :: Seq Clean }

instance Semigroup Cleaner where
  (<>) = (Cleaner .) . (. unCleaner) . (><) . unCleaner

instance Monoid Cleaner where
  mempty = Cleaner mempty

data Clean = forall a . Clean (a -> IO ()) a

newtype CleanerT a = CleanerT { unCleanerT :: (Cleaner, a) }

newtype CleanerT' m a = CleanerT' { unCleanerT' :: forall r . ((Cleaner, a) -> m r) -> m r}

instance Functor CleanerT where
  fmap = (. unCleanerT) . (CleanerT .) . second

instance Applicative CleanerT where
  pure = CleanerT . (mempty ,)
  (<*>) f a = do
    let (x, f') = unCleanerT f
    let (y, a') = unCleanerT a
    CleanerT (x <> y, f' a')

instance Monad CleanerT where
  (>>=) a f = do
    let (x, a') = unCleanerT a
    let (y, b') = unCleanerT (f a')
    CleanerT (x <> y, b')

instance Functor (CleanerT' m) where
  fmap f mx = CleanerT' $ \r ->
    unCleanerT' mx $ \(cleaner, a) ->
    r (cleaner, f a)

instance Applicative (CleanerT' m) where
  pure x = CleanerT' $ \r -> r (mempty, x)
  (<*>) f a = CleanerT' $ \r ->
    unCleanerT' f $ \(x, f') ->
    unCleanerT' a $ \(y, a') ->
    r (x <> y, f' a')

instance Monad (CleanerT' m) where
  (>>=) a f = CleanerT' $ \r ->
    unCleanerT' a $ \(x, a') ->
    unCleanerT' (f a') $ \(y, b) ->
    r (x <> y, b)

instance MonadTrans CleanerT' where
  lift mx = CleanerT' $ \r -> do
    x <- mx
    r (mempty, x)

instance MonadIO m => MonadIO (CleanerT' m) where
  -- liftIO ioa = CleanerT' $ \r -> do
  --   a <- liftIO ioa
  --   r (mempty, a)
  liftIO = lift . liftIO

instance (MonadIO m, MonadUnliftIO m) => MonadUnliftIO (CleanerT' m) where
  withRunInIO f = CleanerT' $ unCleanerT' $ withRunInIO f

runCleanerT' :: MonadIO m => CleanerT' m a -> m a
runCleanerT' ct = unCleanerT' ct $ \(cleaner, res) -> do
  liftIO $ cleanup cleaner
  pure res
  -- let (cio, resio) = bimap cleanup pure $ unCleanerT' ct
  -- _ <- cio
  -- resio

runCleanerT :: CleanerT a -> IO a
runCleanerT ct = do
  let (cio, resio) = bimap cleanup pure $ unCleanerT ct
  _ <- cio
  resio


foo2 :: CleanerT Int
foo2 = do
  _ <- CleanerT (mkCleaner (print "foo2 1"), 1)
  _ <- CleanerT (mkCleaner (print "foo2 2"), 1)
  pure 3

foo3 :: (MonadIO m) => CleanerT' m Int
foo3 = do
  liftIO $ print "io"
  _ <- CleanerT' ($ (mkCleaner (print "foo3 1"), 1))
  _ <- CleanerT' ($ (mkCleaner (print "foo3 2"), 1))
  pure 4

class Monad m => MonadCleaner m where
  liftCleanerT :: CleanerT a -> m a

class MonadIO m => MonadCleaner' m where
  liftCleanerT' :: CleanerT' IO a -> m a

collect' :: (MonadUnliftIO m) => CleanerT' m a -> m (Cleaner, a)
--collect' ma = unCleanerT' ma $ \(cleaner, a) -> pure (cleaner, a)
collect' ma = unCleanerT' ma $ \(cleaner, a) -> withRunInIO $ \run -> 
  ( (run (pure (cleaner, a))))

instance MonadCleaner CleanerT where
  liftCleanerT = id

instance MonadCleaner' (CleanerT' IO) where
  liftCleanerT' ioa = CleanerT' $ \r -> do
    unCleanerT' ioa $ \(cleaner, a) -> r (cleaner, a) 
    -- liftIO $ unCleanerT' ioa $ \(cleaner, a) -> do
    -- _ $ r (cleaner, a)

register :: (MonadCleaner m) => Cleaner -> m ()
register cleaner = liftCleanerT $ CleanerT (cleaner, mempty)

register' :: (MonadCleaner' m) => Cleaner -> m ()
register' cleaner = liftCleanerT' $ CleanerT' $ \r -> r (cleaner, ())

cleaner1 = mkCleaner (print "fu")
cleaner2 = mkCleaner (print "ano")
cleaner3 = mkCleaner (print "three")

foo :: (MonadCleaner m) => m ()
foo = do
  --liftIO $ print "test foo"
  register cleaner1
  register cleaner2
  register cleaner3
  pure ()

foo' :: (MonadCleaner' m) => m ()
foo' = do
  liftIO $ print "test foo'"
  register' cleaner1
  register' cleaner2
  register' cleaner3
  pure ()

bar' :: (MonadIO m) => m ()
bar' = do
  (cleaner, _) <- liftIO $ collect' foo'
  liftIO . cleanup $ cleaner
  pure ()

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire = Acquire

mkCleaner :: IO () -> Cleaner
mkCleaner = Cleaner . singleton . flip Clean () . const

fromAcquire :: MonadIO m => Acquire a -> m (Cleaner, a)
fromAcquire (Acquire acq cleaner) = acquire acq cleaner

acquire :: MonadIO m => IO a -> (a -> IO ()) -> m (Cleaner, a)
acquire  acq cleaner = do
  res <- liftIO acq
  pure (Cleaner . singleton . Clean cleaner $ res, res)

cleanup :: Cleaner -> IO ()
cleanup = foldMap clean . reverse . unCleaner
  where
    clean :: Clean -> IO ()
    clean (Clean cleaner res) = cleaner res
