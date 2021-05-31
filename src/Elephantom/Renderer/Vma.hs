{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elephantom.Renderer.Vma
  ( withAllocator
  , memCopy
  , memCopyU
  ) where

import Vulkan
import Vulkan.Zero
import qualified VulkanMemoryAllocator as Vma

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (sizeOf))
import Foreign.Marshal.Utils (copyBytes, with)

import qualified Data.Vector.Storable as VS

import Control.Monad.Trans.Resource (MonadResource, allocate)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.ApplicationInfo (appInfo)

withAllocator :: MonadResource m => PhysicalDevice -> Device -> Instance -> m Vma.Allocator
withAllocator phys device inst = snd <$> Vma.withAllocator zero
  { Vma.flags = Vma.ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT -- vmaGetBudget
  , Vma.physicalDevice = physicalDeviceHandle phys
  , Vma.device = deviceHandle device
  , Vma.instance' = instanceHandle inst
  , Vma.vulkanApiVersion = apiVersion (appInfo :: ApplicationInfo)
  } allocate

memCopy :: forall a m . (Storable a, MonadIO m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> VS.Vector a -> m ()
memCopy allocator memAllocation datas = do
  bufferMemoryPtr <- Vma.mapMemory allocator memAllocation
  liftIO $ VS.unsafeWith datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral (sizeOf (undefined :: a)) * VS.length datas
  Vma.unmapMemory allocator memAllocation

memCopyU :: forall a m . (Storable a, MonadIO m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> a -> m ()
memCopyU allocator memAllocation datas = do
  bufferMemoryPtr <- Vma.mapMemory allocator memAllocation
  liftIO $ with datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral . sizeOf $ (undefined :: a)
  Vma.unmapMemory allocator memAllocation
