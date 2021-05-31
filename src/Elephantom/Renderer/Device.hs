{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Elephantom.Renderer.Device
  ( withDevice
  ) where

import Vulkan hiding (withDevice)
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan.Requirement
import Vulkan.Utils.Initialization (createDeviceFromRequirements)

import Data.Word (Word32)
import qualified Data.Vector as V
import Data.ByteString.Char8 (ByteString)

import Control.Monad.Trans.Resource (MonadResource)

import Elephantom.Renderer.Util (promote)

withDevice :: MonadResource m => PhysicalDevice -> "queueFamilyIndices" ::: V.Vector Word32 -> m Device
withDevice phys indices = do
  let extensions = [ KHR_SWAPCHAIN_EXTENSION_NAME ]
  let optionals =
        [ EXT_MEMORY_BUDGET_EXTENSION_NAME -- vmaGetBudget
        , KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -- vma use it automatically, promoted to API_VERSION_1_1
        , KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -- dependency of KHR_DEDICATED_ALLOCATION_EXTENSION_NAME, promoted to API_VERSION_1_1
        -- KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
        -- , EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
        -- , KHR_MAINTENANCE3_EXTENSION_NAME
        -- , KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
        -- , KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
        -- , KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
        -- , KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
        -- , EXT_MEMORY_BUDGET_EXTENSION_NAME
        ]
  let optionalFeatures =
        [ RequireDeviceFeature
          { featureName   = "samplerAnisotropy"
          , checkFeature  = samplerAnisotropy :: PhysicalDeviceFeatures -> Bool
          , enableFeature = \f -> f { samplerAnisotropy = True } :: PhysicalDeviceFeatures
          }
        ]
  let deviceCreateInfo :: DeviceCreateInfo '[]
      deviceCreateInfo = zero
        { queueCreateInfos = V.fromList
          [ SomeStruct $ zero
            { queueFamilyIndex = i
            , queuePriorities = [ 1 ]
            }
          | i <- V.toList indices
          ]
        -- , enabledLayerNames = []
        -- , enabledExtensionNames = extensions <> optionals
        }
  createDeviceFromRequirements (require extensions) (require optionals <> optionalFeatures) phys deviceCreateInfo
  where
    require :: "extensions" ::: [ByteString] -> [DeviceRequirement]
    require = map (flip (RequireDeviceExtension Nothing) minBound) . promote
