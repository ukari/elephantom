{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified SDL as SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.CStruct.Extends
import Vulkan hiding (allocate)
import qualified Vulkan.Core10 as Core10
import qualified Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Extensions.VK_EXT_acquire_xlib_display
import Vulkan.Utils.Debug
import Vulkan.Utils.ShaderQQ
import Vulkan.Utils.Initialization
import Vulkan.Requirement
import qualified VulkanMemoryAllocator as Vma

import Foreign.Ptr (Ptr, castPtr)
import Data.Text (Text (..))
import Data.ByteString (packCString)
import Data.Traversable (traverse)
import Data.Bits ((.&.), (.|.), shift, zeroBits)
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Set (union)

import Streamly
import Streamly.Prelude (drain, repeatM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, allocate_, release, register)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Error.Util (hoistMaybe, failWith)
import Control.Exception (Exception (..), throw)

someFunc :: IO ()
someFunc = runResourceT $ do
  -- putStrLn "someFunc"
  --SDL.initialize [SDL.InitVideo]
  --bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit allocate
  withSDL
  window <- withWindow "test" 500 500
  inst <- withInst window
  surf <- withSurface inst window
  phys <- getPhysicalDevice inst
  indices <- findQueueFamilyIndices phys surf
  liftIO $ print indices
  liftIO $ drain $ asyncly $ constRate 60 $ repeatM $ liftIO $ pure ()
  return undefined

type Managed a = forall m . MonadIO m => ResourceT m a

data AppException
  = ImageLoadException String
  | VulkanAllocateMemoryException String
  | VulkanGraphicsFamilyIndexException
  | VulkanPresentFamilyIndexException
  | VulkanLayoutTransitionUnsupport
  deriving (Show)

instance Exception AppException

class AppMaybe m a where
  tryWith :: Monad m => a -> Maybe a -> m a
  tryWithE :: Monad m => AppException -> Maybe a -> m a

instance AppMaybe m a where
  tryWith :: Monad m => a -> Maybe a -> m a
  tryWith normal value = runMaybeT (hoistMaybe value) >>= \case
    Just x -> pure x
    Nothing -> pure normal
  tryWithE :: Monad m => AppException -> Maybe a -> m a
  tryWithE ex value = runExceptT (failWith ex value) >>= \case
    Right x -> pure x
    Left e -> throw e

appInfo :: ApplicationInfo
appInfo = zero { applicationName = Nothing
               , apiVersion = API_VERSION_1_0
               }

withSDL :: Managed ()
withSDL = do
  _sdlInitKey <- allocate_ (SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])) SDL.quit
  _sdlVkKey <- allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  pure ()

withWindow :: Text -> Int -> Int -> Managed SDL.Window
withWindow title width height = do
  (_key, window) <- allocate (SDL.createWindow title SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height)
    , SDL.windowGraphicsContext = SDL.VulkanContext
    })
    SDL.destroyWindow
  pure window

withInst :: SDL.Window -> Managed Instance
withInst window = do
  extensionsCString <- SDL.vkGetInstanceExtensions window
  extensions <- liftIO $ traverse packCString extensionsCString
  inst <- createDebugInstanceFromRequirements [ RequireInstanceExtension Nothing ext minBound | ext <- extensions ] [] zero { applicationInfo = Just appInfo }
  pure inst

withSurface :: Instance -> SDL.Window -> Managed SurfaceKHR
withSurface inst window = do
  (_key, surf) <- allocate (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr $ instanceHandle inst))
    (\s -> destroySurfaceKHR inst s Nothing)
  pure surf

getPhysicalDevice :: MonadIO m => Instance -> m PhysicalDevice
getPhysicalDevice inst = do
  (_, devices) <- enumeratePhysicalDevices inst
  return $ devices ! 0

data QueueFamilyIndices = QueueFamilyIndices
  { graphicsFamily :: !Word
  , presentFamily :: !Word
  } deriving (Eq, Show, Read)

findQueueFamilyIndices :: MonadIO m => PhysicalDevice -> SurfaceKHR -> m QueueFamilyIndices
findQueueFamilyIndices pdevice surf = do
  queueFamilies <- getPhysicalDeviceQueueFamilyProperties pdevice
  graphicsQueueFamilyIndices <- return $ V.map fst $ V.filter isGraphicsQueue $ V.indexed queueFamilies
  presentQueueFamilyIndices <- V.map fst <$> V.filterM isPresentQueue (V.indexed queueFamilies)
  graphicsQueueFamilyIndex <- tryWithE VulkanGraphicsFamilyIndexException (graphicsQueueFamilyIndices !? 0) 
  -- graphicsQueueFamilyIndex <- (return $ graphicsQueueFamilyIndices !? 0) >>= \case
  --    Just e -> return e
  --    Nothing -> throw $ VulkanGraphicsFamilyIndexException
  presentQueueFamilyIndex <- return $ pickPresentFamilyIndex graphicsQueueFamilyIndex presentQueueFamilyIndices
  return $ QueueFamilyIndices (fromIntegral graphicsQueueFamilyIndex) (fromIntegral presentQueueFamilyIndex)
  where
    isGraphicsQueue :: (Int, QueueFamilyProperties) -> Bool
    isGraphicsQueue (_i, q) = QUEUE_GRAPHICS_BIT .&. (queueFlags q) /= zeroBits && (queueCount q > 0)
    isPresentQueue :: MonadIO m => (Int, QueueFamilyProperties) -> m Bool
    isPresentQueue (i, _q) = getPhysicalDeviceSurfaceSupportKHR pdevice (fromIntegral i) surf
    pickPresentFamilyIndex :: "graphicsQueueFamilyIndex" ::: Int -> V.Vector Int -> Int
    pickPresentFamilyIndex gi pis | gi `notElem` pis = case pis !? 0 of
                                      Just e -> e
                                      Nothing -> throw $ VulkanPresentFamilyIndexException
                                  | otherwise = case V.findIndex (/= gi) pis of
                                      Just e -> e
                                      Nothing -> gi

findMemoryType :: MonadIO m => PhysicalDevice -> "memoryTypeBits" ::: Word -> MemoryPropertyFlagBits -> m Word
findMemoryType pdevice typeFilter flagBits = do
  memProps <- getPhysicalDeviceMemoryProperties pdevice
  count <- return $ memoryTypeCount memProps
  memTypes <- return $ memoryTypes memProps
  (return $ V.findIndex matchp (V.indexed $ V.take (fromIntegral count) memTypes)) >>= \case
    Just idx -> return $ fromIntegral idx
    Nothing -> throw $ VulkanAllocateMemoryException "can't allocate video memory"
  where
    matchp :: (Int, MemoryType) -> Bool
    matchp (i, e) = typeFilter .&. (1 `shift` (fromIntegral i)) /= zeroBits && (propertyFlags e) .&. flagBits == flagBits

