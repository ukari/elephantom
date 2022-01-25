{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Elephantom.Renderer.Swapchain
  ( SwapchainResource (..)
  , chooseSharingMode
  , chooseFormat
  , createSwapchain
  , destroySwapchain
  ) where

import Vulkan hiding (createImageView, destroyImageView, createFramebuffer, destroyFramebuffer)
import Vulkan.Zero

import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import qualified Data.Vector as V

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.Util (tryWith)
import Elephantom.Renderer.ImageView (createImageView, destroyImageView)
import Elephantom.Renderer.Framebuffer (createFramebuffer, destroyFramebuffer)

data SwapchainResource = SwapchainResource
  { swapchain :: !SwapchainKHR
  , images :: !(V.Vector Image)
  , imageViews :: !(V.Vector ImageView)
  , framebuffers :: !(V.Vector Framebuffer)
  , extent :: !Extent2D
  }

chooseSharingMode :: "queueFamilyIndices" ::: V.Vector Word32 -> SharingMode
chooseSharingMode indices | length indices == 1 = SHARING_MODE_EXCLUSIVE
                          | otherwise = SHARING_MODE_CONCURRENT

chooseFormat :: V.Vector SurfaceFormatKHR -> SurfaceFormatKHR
chooseFormat formats = fromMaybe (V.head formats) $ V.find isSRGB formats
  where
    isSRGB = (==) SurfaceFormatKHR {format = FORMAT_B8G8R8A8_SRGB, colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR}

createSwapchain :: MonadIO m => PhysicalDevice -> Device -> SurfaceKHR -> SurfaceFormatKHR -> "queueFamilyIndices" ::: V.Vector Word32 -> Extent2D -> RenderPass -> SwapchainKHR -> m SwapchainResource
createSwapchain phys device surf surfaceFormat indices extent renderPass oldSwapchain = do
  (_, presentModes) <- getPhysicalDeviceSurfacePresentModesKHR phys surf
  liftIO $ print presentModes
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surf
  let presentMode = tryWith PRESENT_MODE_FIFO_KHR (V.find (== PRESENT_MODE_MAILBOX_KHR) presentModes)
  let sharingMode = chooseSharingMode indices
  let swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
      swapchainCreateInfo = zero
        { surface = surf
        , minImageCount = minImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) + 1
        , imageFormat = format (surfaceFormat :: SurfaceFormatKHR)
        , imageColorSpace = colorSpace surfaceFormat
        , imageExtent = extent
        , imageArrayLayers = 1
        , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , imageSharingMode = sharingMode
        , queueFamilyIndices = indices
        , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
        , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode = presentMode
        , clipped = True
        , oldSwapchain = oldSwapchain
        }
  swapchain <- createSwapchainKHR device swapchainCreateInfo Nothing
  images <- snd <$> getSwapchainImagesKHR device swapchain
  imageViews <- mapM (createImageView device (format (surfaceFormat :: SurfaceFormatKHR))) images
  liftIO . print $ "imageViews " <> show imageViews
  framebuffers <- mapM (createFramebuffer device extent renderPass) imageViews
  pure SwapchainResource {..}

destroySwapchain :: MonadIO m => Device -> SwapchainResource -> m ()
destroySwapchain device SwapchainResource {..} = do
  mapM_ (destroyImageView device) imageViews
  -- mapM_ (flip (Vulkan.destroyImage device) Nothing) images
  mapM_ (destroyFramebuffer device) framebuffers
  destroySwapchainKHR device swapchain Nothing
