{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Elephantom.Renderer.ImageView
  ( createImageView
  , destroyImageView
  , acquireImageView
  ) where

import Vulkan hiding (createImageView, destroyImageView)
import qualified Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)

import Acquire (acquire, Cleaner)

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

acquireImageView :: MonadIO m =>  Device -> Format -> Image -> m (Cleaner, ImageView)
acquireImageView device format img = acquire (createImageView device format img) (destroyImageView device)
