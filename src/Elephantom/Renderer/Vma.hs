{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elephantom.Renderer.Vma
  ( withAllocator
  , memCopy
  , memCopyV
  , withBuffer
  , withImage
  ) where

import Vulkan hiding (destroyBuffer, destroyImage, withBuffer, withImage)
import Vulkan.Zero
import Vulkan.CStruct.Extends
import qualified VulkanMemoryAllocator as Vma

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (sizeOf))
import Foreign.Marshal.Utils (copyBytes, with)

import qualified Data.Vector.Storable as VS

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.Allocator (Allocator)
import Elephantom.Renderer.ApplicationInfo (appInfo)

withAllocator :: MonadIO m => PhysicalDevice -> Device -> Instance -> Allocator m Vma.Allocator
withAllocator phys device inst = Vma.withAllocator zero
  { Vma.flags = Vma.ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT -- vmaGetBudget
  , Vma.physicalDevice = physicalDeviceHandle phys
  , Vma.device = deviceHandle device
  , Vma.instance' = instanceHandle inst
  , Vma.vulkanApiVersion = apiVersion (appInfo :: ApplicationInfo)
  }

memCopy :: forall a m . (Storable a, MonadIO m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> a -> m ()
memCopy allocator memAllocation datas = do
  bufferMemoryPtr <- Vma.mapMemory allocator memAllocation
  liftIO $ with datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral . sizeOf $ (undefined :: a)
  Vma.unmapMemory allocator memAllocation

memCopyV :: forall a m . (Storable a, MonadIO m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> VS.Vector a -> m ()
memCopyV allocator memAllocation datas = do
  bufferMemoryPtr <- Vma.mapMemory allocator memAllocation
  liftIO $ VS.unsafeWith datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral (sizeOf (undefined :: a)) * VS.length datas
  Vma.unmapMemory allocator memAllocation

withBuffer :: (Extendss BufferCreateInfo a, PokeChain a, MonadIO m) => Vma.Allocator -> BufferCreateInfo a -> Vma.AllocationCreateInfo -> Allocator m (Buffer, Vma.Allocation, Vma.AllocationInfo)
withBuffer allocator bufferCreateInfo allocationCreateInfo allocate = allocate (Vma.createBuffer allocator bufferCreateInfo allocationCreateInfo) destroyBuffer
  where
    destroyBuffer :: MonadIO m => (Buffer, Vma.Allocation, Vma.AllocationInfo) -> m ()
    destroyBuffer (buffer, allocation, _) = Vma.destroyBuffer allocator buffer allocation

withImage :: (Extendss ImageCreateInfo a, PokeChain a, MonadIO m) => Vma.Allocator -> ImageCreateInfo a -> Vma.AllocationCreateInfo -> Allocator m (Image, Vma.Allocation, Vma.AllocationInfo)
withImage allocator imageCreateInfo allocationCreateInfo allocate = allocate (Vma.createImage allocator imageCreateInfo allocationCreateInfo) destroyImage
  where
    destroyImage :: MonadIO m => (Image, Vma.Allocation, Vma.AllocationInfo) -> m ()
    destroyImage (image, allocation, _) = Vma.destroyImage allocator image allocation
