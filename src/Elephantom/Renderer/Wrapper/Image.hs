{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Wrapper.Image
  ( withImageSampled
  ) where

import qualified VulkanMemoryAllocator as Vma
import Vulkan.Zero
import Vulkan hiding (withImage)

import Data.Bits ((.|.))
import Data.Word (Word32)

import Control.Monad.IO.Class (MonadIO)
import Elephantom.Renderer.Command (withSingleTimeCommands)
import Elephantom.Renderer.ImageLayout (transitionImageLayout)
import Elephantom.Renderer.Vma (withImage)
import Elephantom.Renderer.Wrapper.Allocate (Allocate)

withImageSampled :: MonadIO m
                 => Vma.Allocator
                 -> Device
                 -> CommandPool
                 -> Queue
                 -> Format
                 -> "image width" ::: Word32
                 -> "image height" ::: Word32
                 -> Buffer
                 -> Allocate m
                 -> m Image
withImageSampled allocator device transferCommandPool transferQueue textureFormat imageWidth imageHeight stagingBuffer allocate = do
  (textureImage, _textureImageAllocation, _) <- withImage allocator zero
    { imageType = IMAGE_TYPE_2D
    , extent = Extent3D imageWidth imageHeight 1
    , mipLevels = 1
    , arrayLayers = 1
    , format = textureFormat
    , tiling = IMAGE_TILING_OPTIMAL
    , initialLayout = IMAGE_LAYOUT_UNDEFINED
    , usage = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT -- when use staging buffer, VK_IMAGE_USAGE_TRANSFER_DST_BIT is necessary. VUID-VkImageMemoryBarrier-oldLayout-01213
    , samples = SAMPLE_COUNT_1_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_GPU_ONLY
    } allocate
  withSingleTimeCommands device transferCommandPool transferQueue $ \cb -> do
    transitionImageLayout cb textureImage IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    cmdCopyBufferToImage cb stagingBuffer textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      [ zero
        { bufferOffset = 0
        , bufferRowLength = 0
        , bufferImageHeight = 0
        , imageSubresource =  zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , mipLevel = 0
          , baseArrayLayer = 0
          , layerCount = 1
          }
        , imageOffset = Offset3D 0 0 0
        , imageExtent = Extent3D imageWidth imageHeight 1
        } :: BufferImageCopy
      ]
    transitionImageLayout cb textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  pure textureImage
