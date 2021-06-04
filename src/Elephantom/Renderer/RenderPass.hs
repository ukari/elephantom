{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.RenderPass
  ( createRenderPass
  , destroyRenderPass
  , withRenderPass
  ) where

import Vulkan hiding (createRenderPass, destroyRenderPass, withRenderPass)
import qualified Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Allocator (Allocator)

createRenderPass :: MonadIO m => Device -> SurfaceFormatKHR -> m RenderPass
createRenderPass device surfFormat = do
  let colorAttachment = zero
        { format = format (surfFormat :: SurfaceFormatKHR)
        , samples = SAMPLE_COUNT_1_BIT
        , loadOp = ATTACHMENT_LOAD_OP_CLEAR
        , storeOp = ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
        } :: AttachmentDescription
  let colorAttachmentRef = zero
        { attachment = 0
        , layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        } :: AttachmentReference
  let subpass = zero
        { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
        , colorAttachments = [ colorAttachmentRef ]
        } :: SubpassDescription
  Vulkan.createRenderPass device zero
    { attachments = [ colorAttachment ]
    , subpasses = [ subpass ]
    } Nothing

destroyRenderPass :: MonadIO m => Device -> RenderPass -> m ()
destroyRenderPass = flip flip Nothing . Vulkan.destroyRenderPass

withRenderPass :: MonadIO m => Device -> SurfaceFormatKHR -> Allocator m RenderPass
withRenderPass device surfFormat allocate = allocate (createRenderPass device surfFormat) (destroyRenderPass device)
