{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Command
  ( withSingleTimeCommands
  , submitCommand
  ) where

import Vulkan
import Vulkan.Zero
import Vulkan.CStruct.Extends

import qualified Data.Vector as V

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Application (Application (..))
import Elephantom.Renderer.Present (Present (..))
import Elephantom.Renderer.Pipeline (PipelineResource (..))
import Elephantom.Renderer.Viewport (makeViewport)

withSingleTimeCommands :: MonadIO m => Device -> CommandPool -> Queue -> (CommandBuffer -> IO ()) -> m ()
withSingleTimeCommands device commandPool queue f = do
  commandBuffer <- V.head <$> allocateCommandBuffers device zero
    { commandPool = commandPool
    , level = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = 1
    }
  liftIO $ useCommandBuffer commandBuffer zero
    { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } (f commandBuffer)
  fence <- createFence device zero Nothing
  queueSubmit queue
    [ SomeStruct (zero
      { commandBuffers = [ commandBufferHandle commandBuffer ]
      } :: SubmitInfo '[])
    ] fence
  _ <- waitForFences device [ fence ] True maxBound
  freeCommandBuffers device commandPool [ commandBuffer ]
  destroyFence device fence Nothing

submitCommand :: MonadIO m
              => Application
              -> "renderArea" ::: Extent2D
              -> RenderPass
              -> V.Vector Present
              -> (CommandBuffer, Framebuffer)
              -> m ()
submitCommand app extent@Extent2D {..} renderPass presents (commandBuffer, framebuffer) = do
  let viewport = makeViewport app extent
  let viewports = [ viewport ] :: V.Vector Viewport
  liftIO . print $ show viewport
  let scissors =
        [ Rect2D
          { offset = Offset2D 0 0
          , extent = extent
          }
        ] :: V.Vector Rect2D
  liftIO $ useCommandBuffer commandBuffer zero -- do
    { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT -- COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
    } $ cmdUseRenderPass commandBuffer zero
    { renderPass = renderPass
    , framebuffer = framebuffer
    , renderArea = Rect2D
      { offset = zero
      , extent = extent
      }
    , clearValues = [ Color $ Float32 1 1 1 1 ]
    } SUBPASS_CONTENTS_INLINE $ mapM_ (presentCmd viewports scissors) presents
  where
    presentCmd :: V.Vector Viewport -> V.Vector Rect2D -> Present -> IO ()
    presentCmd viewports scissors Present {..} = do
      cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS ((pipeline :: PipelineResource -> Pipeline) pipelineResource)
      let offsets = 0 <$ vertexBuffers
      cmdSetViewport commandBuffer 0 viewports
      cmdSetScissor commandBuffer 0 scissors
      cmdBindVertexBuffers commandBuffer 0 vertexBuffers offsets
      cmdBindIndexBuffer commandBuffer indexBuffer 0 INDEX_TYPE_UINT32
      cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS ((pipelineLayout :: PipelineResource -> PipelineLayout) pipelineResource) 0 descriptorSets []
      cmdDrawIndexed commandBuffer drawSize 1 0 0 0
      --cmdDraw commandBuffer drawSize 1 0 0
