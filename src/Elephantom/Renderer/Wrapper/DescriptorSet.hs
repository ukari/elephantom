{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Wrapper.DescriptorSet
  ( withUniformBufferDescriptorSet
  , withCombinedImageSamplerDescriptorSet
  , withSamplerBufferDescriptorSet
  ) where

import qualified VulkanMemoryAllocator as Vma
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan hiding (withBuffer, withImageView, withBufferView)

import Data.Word (Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.Storable (Storable (sizeOf))

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.CommandPool (CommandPoolResource (..))
import Elephantom.Renderer.Descriptor (DescriptorSetResource (..))
import Elephantom.Renderer.Queue (QueueResource (..))
import Elephantom.Renderer.Wrapper.Buffer (withTransferBuffer, withUniformBuffer, withSamplerBuffer)
import Elephantom.Renderer.Wrapper.Image (withImageSampled)
import Elephantom.Renderer.BufferView (withBufferView)
import Elephantom.Renderer.ImageView (withImageView)
import Elephantom.Renderer.Sampler (withTextureSampler)
import Elephantom.Renderer.Wrapper.Allocate (Allocate)

withUniformBufferDescriptorSet :: forall a m . (Storable a, MonadIO m)
                               => Vma.Allocator
                               -> ("queueFamilyIndices" ::: V.Vector Word32)
                               -> DescriptorSetResource
                               -> "descriptorSet number" ::: Word32
                               -> "binding number" ::: Word32
                               -> a
                               -> Allocate m
                               -> m (SomeStruct WriteDescriptorSet)
withUniformBufferDescriptorSet allocator familyIndices descriptorSetResource setNumber bindingNumber uniform allocate = do
  let texelDataSize = fromIntegral $ sizeOf (undefined :: a)
  uniformBuffer <- withUniformBuffer allocator familyIndices uniform allocate
  let bufferInfos :: V.Vector DescriptorBufferInfo
      bufferInfos =
        [ zero
          { buffer = uniformBuffer
          , offset = 0
          , range = texelDataSize -- WHOLE_SIZE
          } :: DescriptorBufferInfo
        ]
  pure . SomeStruct $ zero
    { dstSet = descriptorSets (descriptorSetResource :: DescriptorSetResource) V.! fromIntegral setNumber
    , dstBinding = bindingNumber
    , dstArrayElement = 0
    , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
    , descriptorCount = fromIntegral . length $ bufferInfos
    , bufferInfo = bufferInfos
    , imageInfo = []
    , texelBufferView = []
    }

withCombinedImageSamplerDescriptorSet :: (Storable a, MonadIO m)
                                      => Vma.Allocator
                                      -> PhysicalDevice
                                      -> Device
                                      -> QueueResource
                                      -> CommandPoolResource
                                      -> DescriptorSetResource
                                      -> "descriptorSet number" ::: Word32
                                      -> "binding number" ::: Word32
                                      -> Format
                                      -> "image width" ::: Word32
                                      -> "image height" ::: Word32
                                      -> VS.Vector a
                                      -> Allocate m
                                      -> m (SomeStruct WriteDescriptorSet)
withCombinedImageSamplerDescriptorSet allocator phys device queueResource commandPoolResource descriptorSetResource setNumber bindingNumber textureFormat imageWidth imageHeight imageData allocate = do
  let transQueue = transferQueue queueResource
  let transCommandPool = transferCommandPool commandPoolResource
  textureSampler <- withTextureSampler phys device allocate
  stagingBuffer <- withTransferBuffer allocator imageData allocate
  img <- withImageSampled allocator device transCommandPool transQueue textureFormat imageWidth imageHeight stagingBuffer allocate
  imgView <- withImageView device textureFormat img allocate
  pure . SomeStruct $ zero
    { dstSet = descriptorSets (descriptorSetResource :: DescriptorSetResource) V.! fromIntegral setNumber
    , dstBinding = bindingNumber
    , dstArrayElement = 0
    , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    , descriptorCount = 1
    , imageInfo =
      [ zero
        { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        , imageView = imgView
        , sampler = textureSampler
        }
      ]
    }

withSamplerBufferDescriptorSet :: forall a m . (Storable a, MonadIO m)
                               => Vma.Allocator
                               -> Device
                               -> QueueResource
                               -> CommandPoolResource
                               -> ("queueFamilyIndices" ::: V.Vector Word32)
                               -> DescriptorSetResource
                               -> "descriptorSet number" ::: Word32
                               -> "binding number" ::: Word32
                               -> Format
                               -> VS.Vector a
                               -> Allocate m
                               -> m (SomeStruct WriteDescriptorSet)
withSamplerBufferDescriptorSet allocator device queueResource commandPoolResource familyIndices descriptorSetResource setNumber bindingNumber texelFormat texelData allocate = do
  let transQueue = transferQueue queueResource
  let transCommandPool = transferCommandPool commandPoolResource
  let texelDataSize = fromIntegral $ sizeOf (undefined :: a) * VS.length texelData
  liftIO . print $ "inner 0"
  samplerBuffer <- withSamplerBuffer allocator device transCommandPool transQueue familyIndices texelData allocate
  samplerBufferView <- withBufferView device texelFormat samplerBuffer allocate
  liftIO . print $ "inner 1"
  let bufferInfos :: V.Vector DescriptorBufferInfo
      bufferInfos =
        [ zero
          { buffer = samplerBuffer
          , offset = 0
          , range = texelDataSize -- WHOLE_SIZE
          } :: DescriptorBufferInfo
        ]
  pure . SomeStruct $ zero
    { dstSet = descriptorSets (descriptorSetResource :: DescriptorSetResource) V.! fromIntegral setNumber
    , dstBinding = bindingNumber
    , dstArrayElement = 0
    , descriptorType = DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
    , descriptorCount = fromIntegral . length $ bufferInfos
    , bufferInfo = bufferInfos
    , imageInfo = []
    , texelBufferView = [ samplerBufferView ]
    }
