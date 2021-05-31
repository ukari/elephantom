{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.ImageLayout
  ( transitionImageLayout
  ) where

import Vulkan
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.RendererException (RendererException (..))

transitionImageLayout :: MonadIO m => CommandBuffer -> Image -> "oldLayout" ::: ImageLayout -> "newLayout" ::: ImageLayout -> m ()
transitionImageLayout commandBuffer image oldLayout newLayout = do
  let barrier = zero
        { oldLayout
        , newLayout
        , image
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , subresourceRange = zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
        }
  case (oldLayout, newLayout) of
    (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
      cmdPipelineBarrier commandBuffer PIPELINE_STAGE_TOP_OF_PIPE_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] []
      [ SomeStruct (barrier
        { srcAccessMask = zero
        , dstAccessMask = ACCESS_TRANSFER_WRITE_BIT
        } :: ImageMemoryBarrier '[]) ]
    (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
      cmdPipelineBarrier commandBuffer PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero [] []
      [ SomeStruct (barrier
        { srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
        , dstAccessMask = ACCESS_SHADER_READ_BIT
        } :: ImageMemoryBarrier '[]) ]
    _ -> throw VulkanLayoutTransitionUnsupport
