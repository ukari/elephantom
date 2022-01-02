{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elephantom.Renderer.Wrapper.TestAllocator
  (
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

data ReleaseKey = ReleaseKey

allocate :: Monad m => IO a -> (a -> IO ()) -> m (ReleaseKey, a)
allocate = undefined

data Acquire a = Acquire a

acquireT :: Monad m => IO r -> (r -> IO ()) -> m r
acquireT = undefined

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire = undefined

type Allocator1 m a = forall r . (m a -> (a -> m ()) -> r) -> r
type Allocator2 m = forall r . m r -> (r -> m ()) -> m r

data Buffer = Buffer deriving (Show)

createBuffer :: IO Buffer
createBuffer = pure Buffer

destroyBuffer :: Buffer -> IO ()
destroyBuffer buffer = pure ()

withBuffer :: MonadIO m => (m Buffer -> (Buffer -> m ()) -> r) -> r
withBuffer allocate = allocate (liftIO createBuffer) (liftIO . destroyBuffer)

data Image = Image deriving (Show)

createImage :: IO Image
createImage = pure Image

destroyImage :: Image -> IO ()
destroyImage image = pure ()

-- withImage :: MonadIO m => (m Image -> (Image -> m ()) -> r) -> r
withImage :: MonadIO m => Allocator1 m Image
withImage allocate = allocate (liftIO createImage) (liftIO . destroyImage)

data Info = Info

makeInfo :: Buffer -> Image -> Info
makeInfo buf img = Info

withResourceWrapper1 :: MonadIO m => Allocator1 m Buffer
withResourceWrapper1 allocate = do
  -- buf <- withBuffer allocate
  -- img <- withImage allocate
  undefined

withResourceWrapper2 :: MonadIO m => Allocator2 m -> m Info
withResourceWrapper2 allocate = do
  buf <- withBuffer allocate
  img <- withImage allocate
  let info = makeInfo buf img
  pure info
