{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.ImageView
  ( createImageView
  , destroyImageView
  , withImageView
  ) where

import Vulkan hiding (createImageView, destroyImageView, withImageView)
import qualified Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Allocator (Allocator)

createImageView :: MonadIO m => Device -> Format -> Image -> m ImageView
createImageView device format img = Vulkan.createImageView device zero
  { image = img
  , viewType = IMAGE_VIEW_TYPE_2D
  , format = format
  , components = zero
    { r = COMPONENT_SWIZZLE_IDENTITY
    , g = COMPONENT_SWIZZLE_IDENTITY
    , b = COMPONENT_SWIZZLE_IDENTITY
    , a = COMPONENT_SWIZZLE_IDENTITY
    }
  , subresourceRange = zero
    { aspectMask = IMAGE_ASPECT_COLOR_BIT
    , baseMipLevel = 0
    , levelCount = 1
    , baseArrayLayer = 0
    , layerCount = 1
    }
  } Nothing

destroyImageView :: MonadIO m => Device -> ImageView -> m ()
destroyImageView = flip flip Nothing . Vulkan.destroyImageView

withImageView :: MonadIO m => Device -> Format -> Image -> Allocator m ImageView
withImageView device format img allocate = allocate (createImageView device format img) (destroyImageView device)
