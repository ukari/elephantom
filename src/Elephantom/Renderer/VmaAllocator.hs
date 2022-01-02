{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.VmaAllocator
  ( VmaAllocator
  , VmaAllocator'
  ) where

import qualified VulkanMemoryAllocator as Vma

import Control.Monad.IO.Class (MonadIO)

-- type VmaAllocator m a = forall r . MonadIO r => (m (a, Vma.Allocation, Vma.AllocationInfo) -> ((a, Vma.Allocation, Vma.AllocationInfo) -> m ()) -> r (a, Vma.Allocation, Vma.AllocationInfo)) -> r a

type VmaAllocator m a = m (a, Vma.Allocation, Vma.AllocationInfo) -> ((a, Vma.Allocation, Vma.AllocationInfo) -> m ()) -> m a

type VmaAllocator' m a b = forall r . MonadIO r => (m (a, Vma.Allocation, Vma.AllocationInfo) -> ((a, Vma.Allocation, Vma.AllocationInfo) -> m ()) -> r (a, Vma.Allocation, Vma.AllocationInfo)) -> r b



