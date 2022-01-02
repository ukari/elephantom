{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Wrapper.Buffer
  ( withVertexBuffer
  , withIndexBuffer
  , withTransferBuffer
  , withUniformBuffer
  ) where

import qualified VulkanMemoryAllocator as Vma
import Vulkan.Zero
import Vulkan hiding (withBuffer)

import Data.Word (Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.Storable (Storable (sizeOf))

import Control.Monad.IO.Class (MonadIO)
import Elephantom.Renderer.Swapchain (chooseSharingMode)
import Elephantom.Renderer.Vma (withBuffer, memCopy, memCopyV)

import Elephantom.Renderer.Wrapper.Allocate (Allocate)

withVertexBuffer :: forall a m .
                    (Storable a, MonadIO m)
                 => Vma.Allocator
                 -> VS.Vector a
                 -> Allocate m
                 -> m Buffer
withVertexBuffer allocator vertices allocate = do
  (verticesBuffer, verticesBufferAllocation, _) <- withBuffer allocator zero
    { size = fromIntegral $ sizeOf (undefined :: a) * VS.length vertices
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  memCopyV allocator verticesBufferAllocation vertices
  pure verticesBuffer

withIndexBuffer :: forall a m .
                   (Storable a, MonadIO m)
                => Vma.Allocator
                -> VS.Vector a
                -> Allocate m
                -> m Buffer
withIndexBuffer allocator indices allocate = do
  (indicesBuffer, indicesBufferAllocation, _) <- withBuffer allocator zero
    { size = fromIntegral $ sizeOf (undefined :: a) * VS.length indices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  memCopyV allocator indicesBufferAllocation indices
  pure indicesBuffer

withTransferBuffer :: forall a m .
                      (Storable a, MonadIO m)
                   => Vma.Allocator
                   -> VS.Vector a
                   -> Allocate m
                   -> m Buffer
withTransferBuffer allocator transData allocate = do
  (stagingBuffer, stagingBufferAllocation, _) <- withBuffer allocator zero
    { size = fromIntegral $ sizeOf (undefined :: a) * VS.length transData
    , usage = BUFFER_USAGE_TRANSFER_SRC_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_ONLY
    } allocate
  memCopyV allocator stagingBufferAllocation transData
  pure stagingBuffer

withUniformBuffer :: forall a m .
                     (Storable a, MonadIO m)
                  => Vma.Allocator
                  -> "queueFamilyIndices" ::: V.Vector Word32 
                  -> a
                  -> Allocate m
                  -> m (V.Vector DescriptorBufferInfo)
withUniformBuffer allocator familyIndices uniformData allocate = do
  (uniformBuffer, uniformBufferAllocation, _) <- withBuffer allocator zero
    { size = fromIntegral $ sizeOf (undefined :: a)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode familyIndices
    , queueFamilyIndices = familyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  memCopy allocator uniformBufferAllocation uniformData
  let bufferInfos :: V.Vector DescriptorBufferInfo
      bufferInfos =
        [ zero
          { buffer = uniformBuffer
          , offset = 0
          , range = fromIntegral . sizeOf $ (undefined :: a)
          } :: DescriptorBufferInfo
        ]
  pure bufferInfos
