{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Framebuffer
  ( createFramebuffer
  , destroyFramebuffer
  ) where

import Vulkan hiding (createFramebuffer, destroyFramebuffer)
import qualified Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)

createFramebuffer :: MonadIO m => Device -> Extent2D -> RenderPass -> ImageView -> m Framebuffer
createFramebuffer device curExtent renderPass imageView = Vulkan.createFramebuffer device zero
  { renderPass
  , attachments = [ imageView ]
  , width = width (curExtent :: Extent2D)
  , height = height (curExtent :: Extent2D)
  , layers = 1
  } Nothing

destroyFramebuffer :: MonadIO m => Device -> Framebuffer -> m ()
destroyFramebuffer = flip flip Nothing . Vulkan.destroyFramebuffer
