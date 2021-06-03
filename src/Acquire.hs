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

data Acquire a = Acquire (IO a) (a -> IO ())

newtype Cleaner = Cleaner { unCleaner :: Seq Clean }

data Clean = forall a . Clean (a -> IO ()) a

newtype CleanerT a = CleanerT { unCleanerT :: (Cleaner, a) }

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

runCleanerT :: CleanerT a -> IO a
runCleanerT ct = do
  let (cio, resio) = bimap (cleanup . Cleaner . reverse . unCleaner) pure $ unCleanerT ct
  _ <- cio
  resio

-- register :: Cleaner -> CleanerT ()
-- register cleaner = CleanerT (cleaner, mempty)

foo2 :: CleanerT Int
foo2 = do
  _ <- CleanerT (mkCleaner (print "fuck"), 1)
  _ <- CleanerT (mkCleaner (print "fuck2"), 1)
  pure 3

class Monad m => MonadCleaner m where
  register ::  Cleaner -> CleanerT ()


cleaner1 = mkCleaner (print "fuck")
cleaner2 = mkCleaner (print "ano")

-- foo :: (MonadCleaner m, MonadIO m) => m ()
-- foo = do
--   register cleaner1
--   -- tell cleaner2
--   -- tell cleaner2
--   pure ()

-- -- foo2 :: (MonadCleanable m, MonadIO m) => m ()
-- -- foo2 = foo >> tell cleaner2

-- bar = do
--   (cleaner, _) <- runWriter foo
--   cleanup . Cleaner . reverse . unCleaner $ cleaner

instance Semigroup Cleaner where
  (<>) = (Cleaner .) . (. unCleaner) . (><) . unCleaner

instance Monoid Cleaner where
  mempty = Cleaner mempty

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
cleanup = foldMap clean . unCleaner
  where
    clean :: Clean -> IO ()
    clean (Clean cleaner res) = cleaner res
