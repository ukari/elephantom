{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.Allocator
  ( Allocator
  , VmaAllocator
  ) where

import qualified VulkanMemoryAllocator as Vma

import Control.Monad.IO.Class (MonadIO)

type Allocator m a = forall r . (m a -> (a -> m ()) -> r) -> r

type VmaAllocator m a = forall r . MonadIO r => (m (a, Vma.Allocation, Vma.AllocationInfo) -> ((a, Vma.Allocation, Vma.AllocationInfo) -> m ()) -> r (a, Vma.Allocation, Vma.AllocationInfo)) -> r a
