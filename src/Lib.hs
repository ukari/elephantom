-- record field
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- type
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- sugar
--{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified SDL as SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan.Zero
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
import Codec.Picture(PixelRGBA8( .. ), readImage, imageData)
import Graphics.Rasterific (renderDrawing, rectangle, fill)
--import Graphics.Rasterific.Texture

import Language.Haskell.TH hiding (location)
import Type.Reflection (SomeTypeRep, splitApps, typeOf)
import GHC.Generics (Generic)
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr)
import Foreign.Storable (Storable (sizeOf, alignment))
import qualified Foreign.Storable as Storable
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Utils (copyBytes, with, fillBytes)
import Linear ((!*!), V2 (..), V3 (..), V4 (..), M44, Quaternion (..), Epsilon, transpose, identity, lookAt, ortho, inverseOrtho, mkTransformation, axisAngle, m33_to_m44, scaled)

import qualified Linear as Linear
import Data.String (IsString)
import Data.Word (Word32)
import Data.Text (Text (..))
import Data.ByteString (packCString, pack)
import Data.ByteString.Char8 (ByteString)
import Data.Traversable (traverse)
import Data.Bits ((.&.), (.|.), shift, zeroBits)
import Data.Vector ((!), (!?), uniq, modify)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
--import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Algorithms.Intro as V
--import Data.Set (Set, union)
--import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Bool (bool)
import Data.List (group, sort)

import Streamly
import Streamly.Prelude (drain, yield, repeatM)
import qualified Streamly.Prelude as S
import Control.Arrow ((&&&))
import Control.Applicative ((<|>), Applicative (..), optional)
import Control.Monad (join)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT, allocate, allocate_, release, register, liftResourceT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Error.Util (hoistMaybe, failWith)
import Control.Exception (Exception (..), throw)
import Control.Concurrent (forkIO, forkOS)

import Shader
import Offset

appInfo :: ApplicationInfo
appInfo = zero { applicationName = Nothing
               , apiVersion = API_VERSION_1_0
               }

promote :: [ByteString] -> [ByteString]
promote = filter $ p . promoteTo
  where
    p :: Maybe Word32 -> Bool
    p (Just promoteVersion) = apiVersion (appInfo :: ApplicationInfo) < promoteVersion
    p Nothing = True

promoteTo :: ByteString -> Maybe Word32
promoteTo = \case
  KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -> Just API_VERSION_1_1
  _ -> Nothing

makeRes :: MonadResource m => a -> m a
makeRes x = do
  allocate_ (liftIO . pure $ x) (liftIO . print $ "release")
  pure x

step :: (MonadIO m) => Int -> m (Maybe Int)
step x = do
  liftIO . print $ "x is : " <> show x
  if (x < 10)
    then pure . Just $ x + 1
    else pure Nothing

testRes :: IO ()
testRes = runResourceT $ do
  res <- makeRes 1
  let fps = 1
  liftIO . S.drainWhile isJust . S.drop 1 . asyncly . constRate fps . S.iterateM (maybe (pure Nothing) step) . pure . Just $ res

ortho' :: (Num a, Floating a) => a -> a -> a -> a -> a -> a -> M44 a
ortho' left right bottom top near far = V4
  (V4 (2/(right - left)) 0 0 (-(right + left)/(right - left)))
  (V4 0 (2/(top - bottom)) 0 (-(top + bottom)/(top - bottom)))
  (V4 0 0 (-2/(far - near)) (-(far + near)/(far - near)))
  (V4 0 0 0 1)
  

ortho2D :: (Num a, Floating a) => a -> a -> a -> a -> M44 a
ortho2D left right bottom top = ortho' left right bottom top (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))

rotateAt :: (Num a, Epsilon a, Floating a) => V3 a -> Quaternion a -> M44 a
rotateAt (V3 x y z) quaternion = mkTransformation quaternion (V3 (0+(x)) (0+(y)) (0+(z))) !*! mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 (-x) (-y) (-z))

someFunc :: IO ()
someFunc = runResourceT $ do
  
  withSDL
  window <- withWindow "test" 500 500
  inst <- withInst window
  surf <- withSurface inst window
  phys <- getPhysicalDevice inst
  --liftIO . print =<< getPhysicalDeviceProperties phys
  qIndices <- findQueueFamilyIndices phys surf
  liftIO $ print qIndices
  let queueFamilyIndices = uniq . modify V.sort $ fmap ($ qIndices) [graphicsFamily, presentFamily]
  device <- Lib.withDevice phys queueFamilyIndices
  liftIO $ print $ "device " <> show (deviceHandle device)
  graphicsQueue <- getDeviceQueue device (graphicsFamily qIndices) 0
  presentQueue <- getDeviceQueue device (presentFamily qIndices) 0
  SwapchainInfo {..} <- withSwapchain phys surf device queueFamilyIndices (Extent2D 500 500)
  shaderStageInfo@ShaderStageInfo {..} <- withShaderStages device
  PipelineResource {..} <- Lib.withPipeline device renderPass shaderStageInfo
  (_, commandPool) <- withCommandPool device zero
    { queueFamilyIndex = graphicsFamily qIndices
    , flags = zeroBits
    } Nothing allocate
  commandBuffers <- Lib.withCommandBuffers device commandPool framebuffers
  
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/vk__mem__alloc_8h.html#a4f87c9100d154a65a4ad495f7763cf7c
  allocator <- snd <$> Vma.withAllocator zero
    { Vma.flags = Vma.ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT -- vmaGetBudget
    , Vma.physicalDevice = physicalDeviceHandle phys
    , Vma.device = deviceHandle device
    , Vma.instance' = instanceHandle inst
    , Vma.vulkanApiVersion = apiVersion (appInfo :: ApplicationInfo)
    } allocate
  (vertexBuffer, vertexBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ 3 * sizeOf (undefined :: ShaderInputVertex)
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT -- support vkCmdBindVertexBuffers.pBuffers
    , sharingMode = SHARING_MODE_EXCLUSIVE -- chooseSharingMode queueFamilyIndices
    -- , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU--GPU_ONLY
    } allocate
  let vertices =
        [ ShaderInputVertex (V2 250 125) (V3 (102/255) (53/255) (53/255))
        , ShaderInputVertex (V2 375 (375)) (V3 (53/255) (102/255) (53/255))
        , ShaderInputVertex (V2 (125) (375)) (V3 (53/255) (53/255) (102/255))
        ] :: VS.Vector ShaderInputVertex
  runResourceT $ memCopy allocator vertexBufferAllocation vertices -- early free

  let indices = [0, 1, 2] :: VS.Vector Word32
  (indexBuffer, indexBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (indices VS.! 0) * VS.length indices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU--Vma.MEMORY_USAGE_GPU_ONLY
    } allocate
  runResourceT $ memCopy allocator indexBufferAllocation indices

  (uniformBuffer, uniformBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ 1 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU --GPU_ONLY
    } allocate
  let uniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (500) (0) (500) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (500/2*0.5) (500/2*0.5) 0) (axisAngle (V3 0 0 1) (45/360*2*pi)) !*! (m33_to_m44 . scaled $ 0.5)
        }
  runResourceT $ memCopyU allocator uniformBufferAllocation uniform -- early free
  -- https://www.reddit.com/r/vulkan/comments/8u9zqr/having_trouble_understanding_descriptor_pool/e1e8d5f?utm_source=share&utm_medium=web2x&context=3
  -- https://www.reddit.com/r/vulkan/comments/clffjm/descriptorpool_maxsets_how_does_this_work_if_you/
  -- https://www.reddit.com/r/vulkan/comments/aij7zp/there_is_a_good_technique_to_update_a_vertex/
  descriptorPool <- snd <$> withDescriptorPool device zero
    { poolSizes =
        [ zero
          { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , descriptorCount = 1
          }
        ]
    , maxSets = fromIntegral . length $ images
    , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT -- VUID-vkFreeDescriptorSets-descriptorPool-00312: descriptorPool must have been created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag
    } Nothing allocate
  descriptorSets <- snd <$> withDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts = descriptorSetLayouts
    } allocate
  liftIO $ print descriptorSets
  let bufferInfos :: V.Vector DescriptorBufferInfo
      bufferInfos =
        [ zero
          { buffer = uniformBuffer
          , offset = 0
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        ]
  updateDescriptorSets device
    [ SomeStruct $ zero
      { dstSet = descriptorSets ! 0
      , dstBinding = 0
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = fromIntegral . length $ bufferInfos
      , bufferInfo = bufferInfos
      , imageInfo = []
      , texelBufferView = []
      }
    ] []
  mapM_ (submitCommand pipeline pipelineLayout extent renderPass [vertexBuffer] indexBuffer descriptorSets (VS.length vertices)) (V.zip commandBuffers framebuffers)

  textureSampler <- withTextureSampler phys device
  textureShaderStages <- Lib.withTextureShaderStages device
  texturePipeline <- Lib.withPipeline device renderPass textureShaderStages
  textureDescriptorPool <- snd <$> withDescriptorPool device zero
    { poolSizes =
        [ zero
          { type' = DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , descriptorCount = 1
          }
        , zero
          { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = 1
          }
        ]
    , maxSets = fromIntegral . length $ images
    , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
    } Nothing allocate
  let texCoords =
        [ Texture (V2 50 50) (V3 0 0 0) (V2 0 0)
        , Texture (V2 250 50) (V3 0 0 0) (V2 0 1)
        , Texture (V2 250 150) (V3 0 0 0) (V2 1 1)
        , Texture (V2 50 150) (V3 0 0 0) (V2 0 1)
        ] :: VS.Vector Texture
  let pixels = renderDrawing 200 100 (PixelRGBA8 255 255 0 255) $ fill $ rectangle (V2 0 0) 200 100
  

  SyncResource {..} <- withSyncResource device framebuffers

  let fps = 60
  let sync = 0
  liftIO . S.drainWhile isJust . S.drop 1 . asyncly . minRate fps . maxRate fps . S.iterateM (maybe (pure Nothing) drawFrame) . pure . Just $ Frame {..}
  return undefined

data Frame = Frame
  { device :: Device
  , swapchain :: SwapchainKHR
  , graphicsQueue :: Queue
  , presentQueue :: Queue
  , imageAvailableSemaphores :: V.Vector Semaphore
  , renderFinishedSemaphores :: V.Vector Semaphore
  , commandBuffers :: V.Vector CommandBuffer
  , sync :: Int
  }

drawFrame :: (MonadIO m) => Frame -> m (Maybe Frame)
drawFrame x@Frame {..} = do
  let imageAvailableSemaphore = imageAvailableSemaphores ! sync
  imageIndex <- snd <$> acquireNextImageKHRSafe device swapchain maxBound imageAvailableSemaphore zero
  --liftIO $ print imageIndex
  queueSubmit graphicsQueue
    [ SomeStruct $ zero
      { Core10.waitSemaphores = [ imageAvailableSemaphores ! sync ]
      , waitDstStageMask = [ PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ]
      , commandBuffers = [ commandBufferHandle (commandBuffers ! fromIntegral imageIndex) ]
      , signalSemaphores = [ renderFinishedSemaphores ! sync ]
      }
    ] zero
  _ <- queuePresentKHR presentQueue zero
    { Swap.waitSemaphores = [ renderFinishedSemaphores ! sync ]
    , swapchains = [ swapchain ]
    , imageIndices = [ imageIndex ]
    }
  queueWaitIdle presentQueue
  queueWaitIdle graphicsQueue
  pure . Just $ x {sync = (sync + 1) `mod` (fromIntegral . length $ commandBuffers)}

type Managed a = forall m . MonadResource m => m a

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
  let optionals =
        [ KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -- the dependency of device extension EXT_MEMORY_BUDGET_EXTENSION_NAME
        ]
  createDebugInstanceFromRequirements (require extensions) (require optionals) zero { applicationInfo = Just appInfo }
  where
    require :: "extensions" ::: [ByteString] -> [InstanceRequirement]
    require = map (flip (RequireInstanceExtension Nothing) minBound) . promote

withSurface :: Instance -> SDL.Window -> Managed SurfaceKHR
withSurface inst window = do
  (_key, surf) <- allocate (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr . instanceHandle $ inst))
    (flip (destroySurfaceKHR inst) Nothing)
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

withDevice :: PhysicalDevice -> "queueFamilyIndices" ::: V.Vector Word32 -> Managed Device
withDevice phys indices = do
  let extensions = [ KHR_SWAPCHAIN_EXTENSION_NAME ]
  let optionals =
        [ EXT_MEMORY_BUDGET_EXTENSION_NAME -- vmaGetBudget
        , KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -- vma use it automatically, promoted to API_VERSION_1_1
        , KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -- dependency of KHR_DEDICATED_ALLOCATION_EXTENSION_NAME, promoted to API_VERSION_1_1
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
          | i <- V.toList $ indices
          ]
        -- , enabledLayerNames = []
        -- , enabledExtensionNames = extensions <> optionals
        }
  createDeviceFromRequirements (require extensions) ((require optionals) <> optionalFeatures) phys deviceCreateInfo
  where
    require :: "extensions" ::: [ByteString] -> [DeviceRequirement]
    require = map (flip (RequireDeviceExtension Nothing) minBound) . promote

data SwapchainInfo = SwapchainInfo
  { swapchain :: SwapchainKHR
  , surfaceFormat :: SurfaceFormatKHR
  , extent :: Extent2D
  , images :: V.Vector Image
  , imageViews :: V.Vector ImageView
  , framebuffers :: V.Vector Framebuffer
  , renderPass :: RenderPass
  }

chooseSharingMode :: "queueFamilyIndices" ::: V.Vector Word32 -> SharingMode
chooseSharingMode indices | length indices == 1 = SHARING_MODE_EXCLUSIVE
                          | otherwise = SHARING_MODE_CONCURRENT

withSwapchain :: PhysicalDevice -> SurfaceKHR -> Device -> "queueFamilyIndices" ::: V.Vector Word32 -> Extent2D -> Managed SwapchainInfo
withSwapchain phys surf device indices extent = do
  (_, formats) <- getPhysicalDeviceSurfaceFormatsKHR phys surf
  let surfaceFormat = formats!0
  liftIO $ print formats
  (_, presentModes) <- getPhysicalDeviceSurfacePresentModesKHR phys surf
  liftIO $ print presentModes
  let presentMode = tryWith PRESENT_MODE_FIFO_KHR (V.find (== PRESENT_MODE_MAILBOX_KHR) presentModes)
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surf
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
        , oldSwapchain = NULL_HANDLE
        }
  swapchain <- snd <$> withSwapchainKHR device swapchainCreateInfo Nothing allocate
  images <- snd <$> getSwapchainImagesKHR device swapchain
  imageViews <- mapM (Lib.withImageView device surfaceFormat) images
  renderPass <- Lib.withRenderPass device surfaceFormat
  framebuffers <- mapM (Lib.withFramebuffer device extent renderPass) imageViews
  pure SwapchainInfo {..}

withImageView :: Device -> SurfaceFormatKHR -> Image -> Managed ImageView
withImageView device surfaceFormat img =
   pure . snd =<< Vulkan.withImageView device zero
     { image = img
     , viewType = IMAGE_VIEW_TYPE_2D
     , format = format (surfaceFormat :: SurfaceFormatKHR)
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
     } Nothing allocate

data ShaderStageInfo = ShaderStageInfo
  { shaderStages :: V.Vector (SomeStruct PipelineShaderStageCreateInfo)
  , descriptorSetLayouts :: V.Vector DescriptorSetLayout
  , descriptorSetLayoutCreateInfos :: V.Vector (DescriptorSetLayoutCreateInfo '[])
  , vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  }

withShaderStages :: Device -> Managed ShaderStageInfo
withShaderStages device = do
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

  layout(location = 0) out vec3 fragColor;

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
  shaderStages <- pure
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
  let descriptorSetLayoutCreateInfos :: V.Vector (DescriptorSetLayoutCreateInfo '[])
      descriptorSetLayoutCreateInfos =
        [ zero
          { bindings =
            [ zero
              { binding = 0
              , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
              , descriptorCount = 1
              , stageFlags = SHADER_STAGE_VERTEX_BIT
              }
            ]
          }
        ]
  descriptorSetLayouts <- mapM (Lib.withDescriptorSetLayout device) descriptorSetLayoutCreateInfos
  -- https://vulkan-tutorial.com/Vertex_buffers/Vertex_input_description
  let vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
      vertexInputState = Just $ SomeStruct $ zero
        { vertexBindingDescriptions =
          [ zero
            { binding = 0
            , stride = fromIntegral . sizeOf $ (undefined :: ShaderInputVertex)
            , inputRate = VERTEX_INPUT_RATE_VERTEX
            }
          ]
        , vertexAttributeDescriptions =
          [ zero
            { binding = 0
            , location = 0
            , offset = offsetof (undefined :: ShaderInputVertex) (0 :: Int)
            , format = FORMAT_R32G32_SFLOAT
            }
          , zero
            { binding = 0
            , location = 1
            , offset = offsetof (undefined :: ShaderInputVertex) ("inColor" :: String)
            , format = FORMAT_R32G32B32_SFLOAT
            }
          ]
        }
  pure ShaderStageInfo {..}

withTextureShaderStages :: Device -> Managed ShaderStageInfo
withTextureShaderStages device = do
  vertCode <- return [vert|
  #version 450
  #extension GL_ARB_separate_shader_objects : enable

  layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo;

  layout(location = 0) in vec2 position;
  layout(location = 1) in vec3 color;
  layout(location = 2) in vec2 texCoord;

  layout(location = 0) out vec3 fragColor;
  layout(location = 1) out vec2 fragTexCoord;

  void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(position, 0.0, 1.0);
    fragColor = color;
    fragTexCoord = texCoord;
  }
  |]
  fragCode <- return [frag|
  #version 450

  #extension GL_ARB_separate_shader_objects : enable

  layout(binding = 1) uniform sampler2D texSampler;

  layout(location = 0) in vec3 fragColor;
  layout(location = 1) in vec2 fragTexCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, fragTexCoord); // for image use texSampler, for shape created by rasterfic use fragColor
  }
  |]
  (_, vertShaderStage) <- withShaderModule device zero { code = vertCode } Nothing allocate
  (_, fragShaderStage) <- withShaderModule device zero { code = fragCode } Nothing allocate
  shaderStages <- pure
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
  let descriptorSetLayoutCreateInfos :: V.Vector (DescriptorSetLayoutCreateInfo '[])
      descriptorSetLayoutCreateInfos =
        [ zero
          { bindings =
            [ zero
              { binding = 0
              , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
              , descriptorCount = 1
              , stageFlags = SHADER_STAGE_VERTEX_BIT
              }
            , zero
              { binding = 1
              , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
              , descriptorCount = 1
              , stageFlags = SHADER_STAGE_FRAGMENT_BIT
              }
            ]
          }
        ]
  descriptorSetLayouts <- mapM (Lib.withDescriptorSetLayout device) descriptorSetLayoutCreateInfos
  let vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
      vertexInputState = Just $ SomeStruct $ zero
        { vertexBindingDescriptions =
          [ zero
            { binding = 0
            , stride = fromIntegral . sizeOf $ (undefined :: Texture)
            , inputRate = VERTEX_INPUT_RATE_VERTEX
            }
          ]
        , vertexAttributeDescriptions =
          [ zero
            { binding = 0
            , location = 0
            , offset = offsetof (undefined :: Texture) ("position" :: String)
            , format = FORMAT_R32G32_SFLOAT
            }
          , zero
            { binding = 0
            , location = 1
            , offset = offsetof (undefined :: Texture) ("color" :: String)
            , format = FORMAT_R32G32B32_SFLOAT
            }
          , zero
            { binding = 0
            , location = 2
            , offset = offsetof (undefined :: Texture) ("texCoord" :: String)
            , format = FORMAT_R32G32B32_SFLOAT
            }
          ]
        }
  pure ShaderStageInfo {..}

withDescriptorSetLayout :: Device -> DescriptorSetLayoutCreateInfo '[] -> Managed DescriptorSetLayout
withDescriptorSetLayout device descriptorSetLayoutCreateInfo =
  snd <$> Vulkan.withDescriptorSetLayout device descriptorSetLayoutCreateInfo Nothing allocate

data DescriptorSetResource = DescriptorSetResource
  { descriptorPool :: !(DescriptorPool)
  , descriptorSets :: !(V.Vector DescriptorSet)
  } deriving (Show)

withDescriptorSetResource :: Device -> "swapchain images" ::: V.Vector Image -> V.Vector DescriptorSetLayout -> DescriptorSetLayoutCreateInfo '[] -> Managed DescriptorSetResource
withDescriptorSetResource device images descriptorSetLayouts descriptorSetLayoutCreateInfo = do
  -- https://www.reddit.com/r/vulkan/comments/8u9zqr/having_trouble_understanding_descriptor_pool/e1e8d5f?utm_source=share&utm_medium=web2x&context=3
  -- https://www.reddit.com/r/vulkan/comments/clffjm/descriptorpool_maxsets_how_does_this_work_if_you/
  -- https://www.reddit.com/r/vulkan/comments/aij7zp/there_is_a_good_technique_to_update_a_vertex/
  let descriptorPoolCreateInfo = makeDescriptorPoolCreateInfo images descriptorSetLayoutCreateInfo
  descriptorPool <- snd <$> withDescriptorPool device descriptorPoolCreateInfo Nothing allocate
  descriptorSets <- snd <$> withDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts = descriptorSetLayouts
    } allocate
  pure DescriptorSetResource {..}

makeDescriptorPoolCreateInfo :: "swapchain images" ::: V.Vector Image -> DescriptorSetLayoutCreateInfo '[] -> DescriptorPoolCreateInfo '[]
makeDescriptorPoolCreateInfo images info = zero
  { poolSizes = V.fromList
    [ zero
      { type' = t
      , descriptorCount = fromIntegral n
      }
    | (t, n) <- analyse info ]
  , maxSets = fromIntegral . length $ images
  , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT -- VUID-vkFreeDescriptorSets-descriptorPool-00312: descriptorPool must have been created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag
  }
  where
    analyse :: DescriptorSetLayoutCreateInfo '[] -> [(DescriptorType, Int)]
    analyse = map (head &&& length) . group . sort . map (descriptorType :: DescriptorSetLayoutBinding -> DescriptorType) . V.toList . bindings

withRenderPass :: Device -> SurfaceFormatKHR -> Managed RenderPass
withRenderPass device surfFormat = do
  colorAttachment <- pure (zero
    { format = format (surfFormat :: SurfaceFormatKHR)
    , samples = SAMPLE_COUNT_1_BIT
    , loadOp = ATTACHMENT_LOAD_OP_CLEAR
    , storeOp = ATTACHMENT_STORE_OP_STORE
    , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
    , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
    , initialLayout = IMAGE_LAYOUT_UNDEFINED
    , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
    } :: AttachmentDescription)
  colorAttachmentRef <- pure (zero
    { attachment = 0
    , layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    } :: AttachmentReference)
  subpass <- pure (zero
    { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
    , colorAttachments = [ colorAttachmentRef ]
    } :: SubpassDescription)
  pure . snd =<< Vulkan.withRenderPass device zero
    { attachments = [ colorAttachment ]
    , subpasses = [ subpass ]
    } Nothing allocate

withFramebuffer :: Device -> Extent2D -> RenderPass -> ImageView -> Managed Framebuffer
withFramebuffer device curExtent renderPass imageView =
  snd <$> Vulkan.withFramebuffer device zero
    { renderPass = renderPass
    , attachments = [ imageView ]
    , width = width (curExtent :: Extent2D)
    , height = height (curExtent :: Extent2D)
    , layers = 1
    } Nothing allocate

data PipelineResource = PipelineResource
  { pipeline :: !Pipeline
  , pipelineLayout :: !PipelineLayout
  } deriving (Show)

withPipeline :: Device -> RenderPass -> ShaderStageInfo -> Managed PipelineResource
withPipeline device renderPass ShaderStageInfo {..} = do
  -- https://stackoverflow.com/questions/56928041/what-is-the-purpose-of-multiple-setlayoutcounts-of-vulkan-vkpipelinelayoutcreate
  -- https://vulkan.lunarg.com/doc/view/1.2.135.0/linux/tutorial/html/08-init_pipeline_layout.html
  (_, pipelineLayout) <- withPipelineLayout device zero
    { setLayouts = descriptorSetLayouts
    } Nothing allocate
  liftIO $ print pipelineLayout
  (_, (_result, pipelines)) <- withGraphicsPipelines device zero
    [ SomeStruct $ zero
      { stages = shaderStages
      , vertexInputState = vertexInputState
      , inputAssemblyState = Just zero
        { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        , primitiveRestartEnable = False
        }
      , viewportState = Just $ SomeStruct zero
        { viewportCount = 1
        , scissorCount = 1
        }
      , rasterizationState = SomeStruct $ zero
        { depthClampEnable = False
        , rasterizerDiscardEnable = False
        , lineWidth = 1
        , polygonMode = POLYGON_MODE_FILL
        , cullMode = CULL_MODE_NONE -- NOTE: for 2D pipeline, no cull mode. while for 3D pipeline, needs set CULL_MODE_BACK_BIT
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
  pure PipelineResource {..}

withTextureSampler :: PhysicalDevice -> Device -> Managed Sampler
withTextureSampler phys device = do
  supportAnisotropy <- samplerAnisotropy <$> getPhysicalDeviceFeatures phys
  maxAnisotropy <- maxSamplerAnisotropy . limits <$> getPhysicalDeviceProperties phys
  snd <$> withSampler device zero
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
    } Nothing allocate

withCommandBuffers :: Device -> CommandPool -> V.Vector Framebuffer -> Managed (V.Vector CommandBuffer)
withCommandBuffers device commandPool framebuffers = do
  commandBuffers <- snd <$> Vulkan.withCommandBuffers device zero
    { commandPool = commandPool
    , level = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = fromIntegral . length $ framebuffers
    } allocate
  pure commandBuffers

submitCommand :: Pipeline -> PipelineLayout
              -> "renderArea" ::: Extent2D -> RenderPass
              -> "vertexBuffers" ::: V.Vector Buffer
              -> "indexBuffer" ::: Buffer
              -> "descriptorSets" ::: V.Vector DescriptorSet
              -> "drawSize" ::: Int
              -> (CommandBuffer, Framebuffer)
              -> Managed ()
submitCommand pipeline pipelineLayout extent@Extent2D {..} renderPass vertexBuffers indexBuffer descriptorSets drawSize (commandBuffer, framebuffer) = do
  useCommandBuffer commandBuffer zero -- do
    { flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT --COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } do
    cmdUseRenderPass commandBuffer zero
      { renderPass = renderPass
      , framebuffer = framebuffer
      , renderArea = Rect2D
        { offset = zero
        , extent = extent
        }
      , clearValues = [ Color $ Float32 1 1 1 1 ]
      } SUBPASS_CONTENTS_INLINE do
        cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
        let offsets = const 0 <$> vertexBuffers
        let viewports =
              [ Viewport
                { x = 0
                , y = 0
                , width = fromIntegral width
                , height = fromIntegral height
                , minDepth = 0
                , maxDepth = 1
                }
              ]
        let scissors =
              [ Rect2D
                { offset = Offset2D 0 0
                , extent = extent
                }
              ]
        cmdSetViewport commandBuffer 0 viewports
        cmdSetScissor commandBuffer 0 scissors
        cmdBindVertexBuffers commandBuffer 0 vertexBuffers offsets
        cmdBindIndexBuffer commandBuffer indexBuffer 0 INDEX_TYPE_UINT32
        cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 descriptorSets []
        cmdDrawIndexed commandBuffer (3) 1 0 0 0
        --cmdDraw commandBuffer 3 1 0 0

data SyncResource = SyncResource
  { imageAvailableSemaphores :: V.Vector Semaphore
  , renderFinishedSemaphores :: V.Vector Semaphore
  }

withSyncResource :: Device -> V.Vector Framebuffer -> Managed (SyncResource)
withSyncResource device framebuffers = do
  imageAvailableSemaphores <- mapM (const makeSemaphore) framebuffers
  renderFinishedSemaphores <- mapM (const makeSemaphore) framebuffers
  pure $ SyncResource {..}
  where
    makeSemaphore = snd <$> withSemaphore device zero Nothing allocate

memCopy :: forall a . Storable a => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> VS.Vector a -> Managed ()
memCopy allocator memAllocation datas = do
  bufferMemoryPtr <- snd <$> Vma.withMappedMemory allocator memAllocation allocate
  liftIO $ VS.unsafeWith datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral ((sizeOf (undefined :: a)) * VS.length datas)

memCopyU :: forall a . Storable a => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> a -> Managed ()
memCopyU allocator memAllocation datas = do
  bufferMemoryPtr <- snd <$> Vma.withMappedMemory allocator memAllocation allocate
  liftIO $ with datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral . sizeOf $ (undefined :: a)
