{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
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
--import Vulkan.Utils.QueueAssignment
import Vulkan.Requirement
import qualified VulkanMemoryAllocator as Vma

import Foreign.Ptr (Ptr, castPtr)
import Data.Word (Word32)
import Data.Text (Text (..))
import Data.ByteString (packCString)
import Data.Traversable (traverse)
import Data.Bits ((.&.), (.|.), shift, zeroBits)
import Data.Vector ((!), (!?), uniq, modify)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sort)
import Data.Vector.Algorithms.Intro as V
--import Data.Set (Set, union)
--import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Streamly
import Streamly.Prelude (drain, yield, repeatM)
import qualified Streamly.Prelude as S
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, allocate_, release, register)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Error.Util (hoistMaybe, failWith)
import Control.Exception (Exception (..), throw)

actor :: (Monad m, Show a, Num a, Ord a) => Maybe a -> m (Maybe a)
actor = \case
  Just x -> do
    --liftIO $ print x
    if x < 10
      then pure $ Just (x + 1)
      else pure $ Just x -- Nothing
  Nothing -> pure $ Just 0

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
  let queueIndices = uniq $ modify sort (fmap ($ indices) [graphicsFamily , presentFamily])
  device <- getDevice phys queueIndices
  swapchain <- withSwapchain phys surf device queueIndices (Extent2D 500 500)
  images <- pure . snd =<< getSwapchainImagesKHR device swapchain
  
  let fps = 60
  liftIO $ S.drainWhile (/=Nothing) $ S.drop 1 $ asyncly $ constRate fps $ S.iterateM actor (pure $Just 2)
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


tryWithM :: Monad m => a -> Maybe a -> m a
tryWithM normal value = runMaybeT (hoistMaybe value) >>= \case
  Just x -> pure x
  Nothing -> pure normal

tryWithEM :: Monad m => AppException -> Maybe a -> m a
tryWithEM ex value = runExceptT (failWith ex value) >>= \case
    Right x -> pure x
    Left e -> throw e

tryWith :: a -> Maybe a -> a
tryWith = fromMaybe

tryWithE :: AppException -> Maybe a -> a
tryWithE ex = \case
  Just x -> x
  Nothing -> throw ex

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
    , SDL.windowResizable = False
    , SDL.windowMode = SDL.Windowed
    , SDL.windowPosition = SDL.Centered
    , SDL.windowBorder = True
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
  { graphicsFamily :: !Word32
  , presentFamily :: !Word32
  } deriving (Eq, Show, Read)

findQueueFamilyIndices :: MonadIO m => PhysicalDevice -> SurfaceKHR -> m QueueFamilyIndices
findQueueFamilyIndices pdevice surf = do
  queueFamilies <- getPhysicalDeviceQueueFamilyProperties pdevice
  graphicsQueueFamilyIndices <- return $ V.map fst $ V.filter isGraphicsQueue $ V.indexed queueFamilies
  presentQueueFamilyIndices <- V.map fst <$> V.filterM isPresentQueue (V.indexed queueFamilies)
  graphicsQueueFamilyIndex <- tryWithEM VulkanGraphicsFamilyIndexException (graphicsQueueFamilyIndices !? 0)
  presentQueueFamilyIndex <- return $ pickPresentFamilyIndex graphicsQueueFamilyIndex presentQueueFamilyIndices
  return $ QueueFamilyIndices (fromIntegral graphicsQueueFamilyIndex) (fromIntegral presentQueueFamilyIndex)
  where
    isGraphicsQueue :: (Int, QueueFamilyProperties) -> Bool
    isGraphicsQueue (_i, q) = QUEUE_GRAPHICS_BIT .&. (queueFlags q) /= zeroBits && (queueCount q > 0)
    isPresentQueue :: MonadIO m => (Int, QueueFamilyProperties) -> m Bool
    isPresentQueue (i, _q) = getPhysicalDeviceSurfaceSupportKHR pdevice (fromIntegral i) surf
    -- prefer to pick present queue which is different from graphics queue
    pickPresentFamilyIndex :: "graphicsQueueFamilyIndex" ::: Int -> V.Vector Int -> Int
    pickPresentFamilyIndex gi pis | gi `notElem` pis = tryWithE VulkanPresentFamilyIndexException (pis !? 0)
                                  | otherwise = tryWith gi (V.findIndex (/= gi) pis)

getDevice :: PhysicalDevice -> "queueFamilyIndices" ::: V.Vector Word32 -> Managed Device
getDevice phys indices = do
  let deviceCreateInfo :: DeviceCreateInfo '[]
      deviceCreateInfo = zero
        { queueCreateInfos = V.fromList
          [ SomeStruct $ zero
            { queueFamilyIndex = i
            , queuePriorities = [1]
            }
          | i <- V.toList $ indices
          ]
        , enabledLayerNames = []
        , enabledExtensionNames = [ KHR_SWAPCHAIN_EXTENSION_NAME ]
        }
  pure . snd =<< withDevice phys deviceCreateInfo Nothing allocate

data SwapchainResource = SwapchainResource
  { swapchain :: SwapchainKHR
  , images :: V.Vector Image
  , imageViews :: V.Vector ImageView
  , framebuffers :: V.Vector Framebuffer
  }

withSwapchain :: PhysicalDevice -> SurfaceKHR -> Device -> "queueFamilyIndices" ::: V.Vector Word32 -> Extent2D -> Managed SwapchainKHR
withSwapchain phys surf device indices imageExtent = do
  (_, formats) <- getPhysicalDeviceSurfaceFormatsKHR phys surf
  let surfaceFormat = formats!0
  liftIO $ print formats
  (_, presentModes) <- getPhysicalDeviceSurfacePresentModesKHR phys surf
  liftIO $ print presentModes
  let presentMode = tryWith PRESENT_MODE_FIFO_KHR (V.find (== PRESENT_MODE_MAILBOX_KHR) presentModes)
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surf
  let sharingMode = if length indices == 1
      then SHARING_MODE_EXCLUSIVE
      else SHARING_MODE_CONCURRENT
  let swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
      swapchainCreateInfo = zero
        { surface = surf
        , minImageCount = minImageCount (surfaceCaps :: SurfaceCapabilitiesKHR) + 1
        , imageFormat = format (surfaceFormat :: SurfaceFormatKHR)
        , imageColorSpace = colorSpace surfaceFormat
        , imageExtent = imageExtent
        , imageArrayLayers = 1
        , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , imageSharingMode = sharingMode
        , queueFamilyIndices = indices
        , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
        , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode = presentMode
        , clipped = True
        , oldSwapchain = NULL_HANDLE
        }
  pure . snd =<< withSwapchainKHR device swapchainCreateInfo Nothing allocate

findMemoryType :: MonadIO m => PhysicalDevice -> "memoryTypeBits" ::: Word -> MemoryPropertyFlagBits -> m Word
findMemoryType pdevice typeFilter flagBits = do
  memProps <- getPhysicalDeviceMemoryProperties pdevice
  count <- return $ memoryTypeCount memProps
  memTypes <- return $ memoryTypes memProps
  return . fromIntegral $ tryWithE (VulkanAllocateMemoryException "can't allocate video memory") (V.findIndex matchp (V.indexed $ V.take (fromIntegral count) memTypes))
  where
    matchp :: (Int, MemoryType) -> Bool
    matchp (i, e) = typeFilter .&. (1 `shift` (fromIntegral i)) /= zeroBits && (propertyFlags e) .&. flagBits == flagBits

createShaders :: Device -> Managed (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createShaders device = do
  vertCode <- return [vert|
  #version 450
  #extension GL_ARB_separate_shader_objects : enable

  layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo;

  layout(location = 0) in vec2 inPosition;
  layout(location = 1) in vec3 inColor;

  layout(location = 3) out vec3 fragColor;

  void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
    fragColor = inColor;
  }
  |]
  fragCode <- return [frag|
  #version 450

  #extension GL_ARB_separate_shader_objects : enable

  layout(location = 0) in vec3 fragColor;

  layout(location = 0) out vec4 outColor; 

  void main() {
    outColor = vec4(fragColor, 1.0);
  }
  |]
  (_, vertShaderStage) <- withShaderModule device zero { code = vertCode } Nothing allocate
  (_, fragShaderStage) <- withShaderModule device zero { code = fragCode } Nothing allocate
  pure
    [ SomeStruct $ zero
      { stage = SHADER_STAGE_VERTEX_BIT
      , module' = vertShaderStage
      , name = "main"
      }
    , SomeStruct $ zero
      { stage = SHADER_STAGE_FRAGMENT_BIT
      , module' = fragShaderStage
      , name = "main"
      }
    ]

createPipeline :: Device -> RenderPass -> V.Vector (SomeStruct PipelineShaderStageCreateInfo) -> Managed Pipeline
createPipeline device renderPass shaderStages = do
  (_, pipelineLayout) <- withPipelineLayout device zero Nothing allocate
  (_, (_result, pipelines)) <- withGraphicsPipelines device zero
    [ SomeStruct $ zero
      { stages = shaderStages
      , vertexInputState = Just zero
      , inputAssemblyState = Just zero
        { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        , primitiveRestartEnable = False
        }
      , viewportState = Just $ SomeStruct zero { viewportCount = 1, scissorCount = 1 }
      , rasterizationState = SomeStruct $ zero
        { depthClampEnable = False
        , rasterizerDiscardEnable = False
        , lineWidth = 1
        , polygonMode = POLYGON_MODE_FILL
        , cullMode = CULL_MODE_NONE
        , frontFace = FRONT_FACE_CLOCKWISE
        , depthBiasEnable = False
        }
      , multisampleState = Just $ SomeStruct $ zero
        { sampleShadingEnable = False
        , rasterizationSamples = SAMPLE_COUNT_1_BIT
        , minSampleShading = 1
        , sampleMask = [ maxBound ]
        }
      , depthStencilState = Nothing
      , colorBlendState = Just $ SomeStruct $ zero
        { logicOpEnable = False
        , attachments =
          [ zero
            { colorWriteMask = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT
            , blendEnable = False
            } ] }
      , dynamicState = Just $ zero
        { dynamicStates =
          [ DYNAMIC_STATE_VIEWPORT
          , DYNAMIC_STATE_SCISSOR ] }
      , layout = pipelineLayout
      , renderPass = renderPass
      , subpass = 0
      , basePipelineHandle = zero
      } ] Nothing allocate
  pipeline <- return $ pipelines ! 0
  return pipeline
