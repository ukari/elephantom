module Elephantom.Renderer.PhysicalDevice
  ( getPhysicalDevice
  ) where

import Vulkan

import Data.Vector ((!))

import Control.Monad.IO.Class (MonadIO)
import Control.Exception (throw)

import Elephantom.Renderer.RendererException (RendererException (..))

getPhysicalDevice :: MonadIO m => Instance -> m PhysicalDevice
getPhysicalDevice inst = do
  (_, devices) <- enumeratePhysicalDevices inst
  if null devices
    then throw VulkanDeviceNotFound
    else pure $ devices ! 0
