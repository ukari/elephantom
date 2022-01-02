{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Wrapper.DescriptorSet
  ( withUniformBufferDescriptorSet
  , withCombinedImageSamplerDescriptorSet
  ) where

import qualified VulkanMemoryAllocator as Vma
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan hiding (withBuffer, withImageView)

import Data.Word (Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.Storable (Storable)

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.CommandPool (CommandPoolResource (..))
import Elephantom.Renderer.Descriptor (DescriptorSetResource (..))
import Elephantom.Renderer.Queue (QueueResource (..))
import Elephantom.Renderer.Wrapper.Buffer (withTransferBuffer, withUniformBuffer)
import Elephantom.Renderer.Wrapper.Image (withImageSampled)
import Elephantom.Renderer.ImageView (withImageView)
import Elephantom.Renderer.Sampler (withTextureSampler)
import Elephantom.Renderer.Allocator (Allocator)
import Elephantom.Renderer.VmaAllocator (VmaAllocator')
import Elephantom.Renderer.Wrapper.Allocate (Allocate)

withUniformBufferDescriptorSet :: (Storable a, MonadIO m)
                               => Vma.Allocator
                               -> ("queueFamilyIndices" ::: V.Vector Word32)
                               -> DescriptorSetResource
                               -> "descriptorSet number" ::: Word32
                               -> "binding number" ::: Word32
                               -> a
                               -> Allocate m
                               -> m (SomeStruct WriteDescriptorSet)
withUniformBufferDescriptorSet allocator familyIndices descriptorSetResource setNumber bindingNumber uniform allocate = do
  bufferInfos <- withUniformBuffer allocator familyIndices uniform allocate
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
