{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Elephantom.Renderer.PhysicalDevice
  ( getPhysicalDevice
  ) where

import Vulkan

import Data.List (sortBy, sortOn, sort)
import Data.Word (Word32)
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Function (on)

import Data.Bifunctor (first, second)
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (throw)

import Elephantom.Renderer.RendererException (RendererException (..))

-- | AMD vendorID
pattern AMD :: Word32
pattern AMD = 0x1002

-- | ImgTec vendorID
pattern ImgTec :: Word32
pattern ImgTec = 0x1010

-- | NVIDIA vendorID
pattern NVIDIA :: Word32
pattern NVIDIA = 0x10DE

-- | ARM vendorID
pattern ARM :: Word32
pattern ARM = 0x13B5

-- | Qualcomm vendorID
pattern Qualcomm :: Word32
pattern Qualcomm = 0x5143

-- | Intel vendorID
pattern Intel :: Word32
pattern Intel = 0x8086

-- | lavapipe vendorID - linux mesa
pattern LavaPipe :: Word32
pattern LavaPipe = 0x10005

-- | hardware gpu vendor
pattern Hardware :: Word32 -> Word32
pattern Hardware vendorID <- (liftA2 (,) isHardware id -> (True, vendorID))

-- | software gpu vendor
pattern Software :: Word32 -> Word32
pattern Software vendorID <- (liftA2 (,) isSoftware id -> (True, vendorID))

-- | unknown gpu vendor
pattern Unknown :: Word32 -> Word32
pattern Unknown vendorID <- (liftA2 (,) isUnknown id -> (True, vendorID))

isHardware :: Word32 -> Bool
isHardware AMD = True
isHardware ImgTec = True
isHardware NVIDIA = True
isHardware ARM = True
isHardware Qualcomm = True
isHardware Intel = True
isHardware _ = False

isSoftware :: Word32 -> Bool
isSoftware LavaPipe = True
isSoftware _ = False

isUnknown :: Word32 -> Bool
isUnknown = not . (fmap (||) isHardware <*> isSoftware)

getPhysicalDevice :: MonadIO m => Instance -> m (Word32, PhysicalDevice)
getPhysicalDevice inst = do
  (_, devices) <- enumeratePhysicalDevices inst
  physDeviceQuerys <- V.mapM queryPhysicalDevice devices
  if null devices
    then throw VulkanDeviceNotFound
    else pure $ preferPhysicalDevices physDeviceQuerys ! 0

queryPhysicalDevice :: MonadIO m => PhysicalDevice -> m (Word32, PhysicalDevice)
queryPhysicalDevice device = liftA2 (,) vendorID (const device) <$> getPhysicalDeviceProperties device

preferPhysicalDevices :: V.Vector (Word32, PhysicalDevice) -> V.Vector (Word32, PhysicalDevice) 
preferPhysicalDevices = V.fromList . sortBy (gpuSort `on` fst) . V.toList

gpuSort :: Word32 -> Word32 -> Ordering
gpuSort (Hardware _) _ = LT
gpuSort (Software _) (Unknown _) = LT
gpuSort _ _ = GT
