{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.QueueFamily
  ( QueueFamilyIndices (..)
  , findQueueFamilyIndices
  ) where

import Vulkan

import Data.Word (Word32)
import Data.Bits ((.&.), (.|.), zeroBits)
import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import Data.List ((\\))

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.Util (tryWithE)
import Elephantom.Renderer.RendererException (RendererException (..))

data QueueFamilyIndices = QueueFamilyIndices
  { graphicsFamily :: !Word32
  , presentFamily :: !Word32
  , transferFamily :: !Word32
  } deriving (Show)

findQueueFamilyIndices :: MonadIO m => PhysicalDevice -> SurfaceKHR -> m QueueFamilyIndices
findQueueFamilyIndices pdevice surf = do
  queueFamilies <- getPhysicalDeviceQueueFamilyProperties pdevice
  liftIO $ print queueFamilies
  let graphicsQueueFamilyIndices = V.map fst . V.filter isGraphicsFamily . V.indexed $ queueFamilies
  presentQueueFamilyIndices <- V.map fst <$> (V.filterM isPresentFamily . V.indexed $ queueFamilies)
  let transferOnlyQueueFamilyIndices = V.map fst . V.filter isTransferOnlyFamily . V.indexed $ queueFamilies
  let transferQueueFamilyIndices = V.map fst . V.filter isTransferFamily . V.indexed $ queueFamilies
  let graphicsFamilyChoose = tryWithE VulkanGraphicsFamilyIndexException $ graphicsQueueFamilyIndices !? 0
  let presentFamilyChoose = tryWithE VulkanPresentFamilyIndexException $ pickPresentFamilyIndices [ graphicsFamilyChoose ] presentQueueFamilyIndices !? 0
  let transferFamilyChoose = if not . null $ transferOnlyQueueFamilyIndices
        then transferOnlyQueueFamilyIndices ! 0
        else tryWithE VulkanPresentFamilyIndexException (pickTransferFamilyIndices [ graphicsFamilyChoose ] [ presentFamilyChoose ] transferQueueFamilyIndices !? 0)
  pure QueueFamilyIndices
    { graphicsFamily = fromIntegral graphicsFamilyChoose
    , presentFamily = fromIntegral presentFamilyChoose
    , transferFamily = fromIntegral transferFamilyChoose
    }
  where
    isGraphicsFamily :: (Int, QueueFamilyProperties) -> Bool
    isGraphicsFamily (_i, q) = QUEUE_GRAPHICS_BIT .&. queueFlags q /= zeroBits && (queueCount q > 0)
    isPresentFamily :: MonadIO m => (Int, QueueFamilyProperties) -> m Bool
    isPresentFamily (i, _q) = getPhysicalDeviceSurfaceSupportKHR pdevice (fromIntegral i) surf
    isTransferFamily :: (Int, QueueFamilyProperties) -> Bool
    isTransferFamily (_i, q) = QUEUE_TRANSFER_BIT .&. queueFlags q /= zeroBits && (queueCount q > 0)
    isTransferOnlyFamily :: (Int, QueueFamilyProperties) -> Bool
    isTransferOnlyFamily (_i, q) = (QUEUE_TRANSFER_BIT .|. QUEUE_GRAPHICS_BIT .|. QUEUE_COMPUTE_BIT) .&. queueFlags q == QUEUE_TRANSFER_BIT && (queueCount q > 0)
    -- prefer to pick present family which is different from selected graphics family
    pickPresentFamilyIndices :: "graphicsQueueFamilyIndices" ::: V.Vector Int -> V.Vector Int -> V.Vector Int
    pickPresentFamilyIndices gis pis = do
      let left = V.toList pis \\ V.toList gis
      if not . null $ left
        then V.fromList left
        else pis
    -- prefer to pick transfer family which is different from selected graphics family and present family
    -- prefer use selected graphics family than selected present family when no choice
    pickTransferFamilyIndices :: "graphicsQueueFamilyIndices" ::: V.Vector Int -> "presentQueueFamilyIndices" ::: V.Vector Int -> V.Vector Int -> V.Vector Int
    pickTransferFamilyIndices gis pis tis = do
      let leftContainG = V.toList tis \\ V.toList pis
      let left = leftContainG \\ V.toList gis 
      if not . null $ left
        then V.fromList left
        else if not . null $ leftContainG
        then V.fromList leftContainG
        else tis
