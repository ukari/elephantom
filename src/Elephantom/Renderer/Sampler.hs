{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Elephantom.Renderer.Sampler
  ( createTextureSampler
  , acquireTextureSampler
  ) where

import Vulkan hiding (destroySampler)
import qualified Vulkan
import Vulkan.Zero

import Data.Bool (bool)

import Control.Monad.IO.Class (MonadIO)

import Acquire (MonadCleaner, acquireT)

createTextureSampler :: MonadIO m => PhysicalDevice -> Device -> m Sampler
createTextureSampler phys device = do
  supportAnisotropy <- samplerAnisotropy <$> getPhysicalDeviceFeatures phys
  maxAnisotropy <- maxSamplerAnisotropy . limits <$> getPhysicalDeviceProperties phys
  createSampler device zero
    { magFilter = FILTER_LINEAR
    , minFilter = FILTER_LINEAR
    , addressModeU = SAMPLER_ADDRESS_MODE_REPEAT -- SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
    , addressModeV = SAMPLER_ADDRESS_MODE_REPEAT -- SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
    , addressModeW = SAMPLER_ADDRESS_MODE_REPEAT -- SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
    , anisotropyEnable = supportAnisotropy
    , maxAnisotropy = bool 1 maxAnisotropy supportAnisotropy
    , borderColor = BORDER_COLOR_INT_TRANSPARENT_BLACK
    , unnormalizedCoordinates = False
    , compareEnable = False
    , compareOp = COMPARE_OP_ALWAYS
    , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
    , mipLodBias = 0
    , minLod = 0
    , maxLod = 0
    } Nothing

{-# INLINE destroySampler #-}
destroySampler :: MonadIO m => Device -> Sampler -> m ()
destroySampler = flip flip Nothing . Vulkan.destroySampler

acquireTextureSampler :: MonadCleaner m => PhysicalDevice -> Device -> m Sampler
acquireTextureSampler phys device = acquireT (createTextureSampler phys device) (destroySampler device)
