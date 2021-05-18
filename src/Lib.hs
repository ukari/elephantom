-- record field
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- type
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

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

import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Text.InterpolatedString.QM (qnb)
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Vulkan hiding (allocate)
import qualified Vulkan.Core10 as Core10
import qualified Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Extensions.VK_EXT_acquire_xlib_display
import Vulkan.Exception
--import Vulkan.Utils.Debug
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import Vulkan.Utils.Initialization (createDebugInstanceFromRequirements, createDeviceFromRequirements)
import Vulkan.Requirement
import qualified VulkanMemoryAllocator as Vma
import Codec.Picture (PixelRGBA8 ( .. ), readImage, imageData)
import qualified Codec.Picture as JP
import Graphics.Rasterific (renderDrawing, rectangle, fill)
import Graphics.Rasterific.Texture (uniformTexture)

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

import qualified Linear
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
import Data.List ((\\), group, sort, sortOn, groupBy)
import Data.Function (on)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Functor ((<&>))
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.StateVar as StateVar
import System.Mem.Weak
import System.Mem.StableName

import Streamly
import Streamly.Prelude (drain, yield, repeatM)
import qualified Streamly.Prelude as S
import Reflex
import qualified Reflex as R
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Reflex.Host.Class (MonadReflexHost, runHostFrame, fireEventsAndRead, fireEvents)
import Reflex.Workflow (Workflow (..))
import Reflex.Network (networkHold)
import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict hiding (modify)
--import Control.Carrier.Error.Church (runError)
--import Control.Effect.Throw (throwError)
--import Control.Effect.Exception (throw, catch, catchJust, try, tryJust)
import Control.Arrow ((&&&))
import Control.Applicative ((<|>), Applicative (..), optional, liftA, liftA2)
import Control.Monad (liftM, liftM2, join, forever)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, ReleaseKey, runResourceT, allocate, allocate_, release, register, unprotect, liftResourceT, resourceForkIO, transResourceT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Error.Util (hoistMaybe, failWith)
import Control.Exception (Exception (..), SomeException (..), throw, handleJust, try, catch)
import qualified Control.Exception as Ex
import Control.Concurrent (forkIO, forkOS, threadDelay)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

import Control.Monad.Logger (runStdoutLoggingT)
-- import Control.Concurrent
import Control.Concurrent.KazuraQueue (newQueue)
import qualified Control.Concurrent.KazuraQueue as KazuraQueue
import Event
import Listen
import GLSL
import Shader
import Offset
import qualified SpirV

testRelease :: IO ()
testRelease = runResourceT $ do
  key <- register (print "hi")
  release key
  liftIO . print $ "end"

type Signal t m = (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))

type Varing t m = (Reflex t, MonadHold t m, MonadFix m)

eventr :: (Signal t m, MonadIO m) => m (R.Event t TickInfo)
eventr = do
  cur <- liftIO getCurrentTime
  tickLossy 6 cur

ticker :: (Signal t m, MonadIO m, Integral a) => a -> m (R.Event t TickInfo)
ticker duration = do
  cur <- liftIO getCurrentTime
  tickLossy (fromIntegral duration) cur

ticker' :: (Signal t m, MonadIO m, Integral a) => a -> m (R.Event t TickInfo)
ticker' duration = do
  cur <- liftIO getCurrentTime
  d <- clockLossy (fromIntegral duration) cur
  pure . updated $ d

tick :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> m (R.Event t TickInfo)
tick durationD = do
  be <- snd <$> runWithReplace (pure ()) (ticker <$> updated durationD)
  initial <- R.sample . current $ durationD
  initialE <- ticker initial
  switchHold initialE be

tick' :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> m (R.Event t TickInfo)
tick' durationD = do
  initial <- R.sample . current $ durationD
  be <- networkHold (ticker initial) (ticker <$> updated durationD)
  pure . switchDyn $ be

test :: IO ()
test = runHeadlessApp $ do
  (tickConfigEvent, tickConfigTrigger) <- newTriggerEvent
  e <- eventr
  dy <- foldDyn (\a b -> ((a + b) `mod` 3) + 1) 0 (1 <$ e)
  tickDyn <- holdDyn 1 tickConfigEvent
  te <- switchDynamic tickDyn ticker'
  performEvent_ $ (\info -> liftIO (print info >> (print =<< getCurrentTime) >> print "frame start" >> threadDelay 2100000 >> (print =<< getCurrentTime) >> print "frame end") ) <$> te
  postBuild <- getPostBuild
  -- performEvent_ $ liftIO . tickConfigTrigger <$> traceEvent "hi" (updated dy)
 -- performEvent_ $ liftIO . print <$> leftmost [postBuild, () <$ te]
  pure never

switchDynamic :: (Signal t m, MonadIO m, Integral a, Adjustable t m) => Dynamic t a -> (a -> m (R.Event t b)) -> m (R.Event t b)
switchDynamic d f = do
  initial <- R.sample . current $ d
  initialE <- f initial
  e <- snd <$> runWithReplace (pure ()) (f <$> updated d)
  switchHold initialE e

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
  EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -> Just API_VERSION_1_2
  KHR_MAINTENANCE3_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME -> Just API_VERSION_1_2
  _ -> Nothing

data EvenException = EvenException deriving (Show)

instance Exception EvenException

makeRes :: (Show a, MonadResource m) => a -> m a
makeRes x = do
  _ <- allocate_ (liftIO . pure $ x) (liftIO . print $ "release " <> show x)
  pure x

step :: (MonadIO m) => (String, Int) -> m (Maybe (String, Int))
step (name, value) = do
  if odd value
    then throw EvenException
    else if value < 10
         then do
           liftIO . print $ name <> " is : " <> show value
           pure . Just $ (name, value + 1)
         else pure Nothing

makeResource :: (MonadResource m) => String -> Int -> m (String, Int)
makeResource name value = do
  name <- makeRes "a"
  value <- makeRes 0
  pure (name, value)

testRes :: IO ()
testRes = do
  let fps = 1
  resCont $ liftIO . S.drainWhile isJust . S.drop 1 . asyncly . constRate fps . S.iterateM (maybe (pure Nothing) step) . pure . Just

resCont :: MonadUnliftIO m => ((String, Int) -> ResourceT m a) -> m a
resCont g = runResourceT $ do
  res <- makeResource "a" 0
  g res

testResH :: (Show r1) => r1 -> (String -> IO ()) -> IO ()
testResH x f = runResourceT $ do
  liftIO $ print $ "use " <> show x
  rtmp <- makeRes ("tmp"::String)
  liftIO . f $ rtmp
  liftIO . print $ "end use " <> show rtmp

testRes2 :: IO ()
testRes2 = runResourceT $ do
  r1 <- makeRes 1
  r2 <- makeRes 2
  liftIO $ testResH r1 $ \rtmp ->
    print $ "use" <> show rtmp
  pure ()

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
  eventQueue <- liftIO newQueue
  withSDL
  window <- withWindow "test" 500 500
  inst <- withInst window
  surf <- withSurface inst window
  phys <- getPhysicalDevice inst
  liftIO . print . maxBoundDescriptorSets . limits =<< getPhysicalDeviceProperties phys
  qIndices <- findQueueFamilyIndices phys surf
  liftIO $ print qIndices
  let queueFamilyIndices = uniq . modify V.sort $ fmap ($ qIndices) [graphicsFamily, presentFamily, transferFamily]
  device <- Lib.withDevice phys queueFamilyIndices
  liftIO $ print $ "device " <> show (deviceHandle device)
  queueRes@QueueResource {..} <- getQueueResource device qIndices
  liftIO $ print $ queueHandle graphicsQueue
  liftIO $ print $ queueHandle presentQueue
  liftIO $ print $ queueHandle transferQueue
  commandPoolRes@CommandPoolResource {..} <- withCommandPoolResource device qIndices
  formats <- snd <$> getPhysicalDeviceSurfaceFormatsKHR phys surf
  let surfaceFormat = chooseFormat formats
  liftIO $ print formats
  liftIO $ print surfaceFormat
  renderPass <- Lib.withRenderPass device surfaceFormat
  
  liftIO . print =<< getPhysicalDeviceSurfaceCapabilitiesKHR phys surf

  -- resource load
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/vk__mem__alloc_8h.html#a4f87c9100d154a65a4ad495f7763cf7c
  allocator <- snd <$> Vma.withAllocator zero
    { Vma.flags = Vma.ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT -- vmaGetBudget
    , Vma.physicalDevice = physicalDeviceHandle phys
    , Vma.device = deviceHandle device
    , Vma.instance' = instanceHandle inst
    , Vma.vulkanApiVersion = apiVersion (appInfo :: ApplicationInfo)
    } allocate

  shaderRes <- withShaderStages device
  pipelineRes <- Lib.withPipeline device renderPass shaderRes
  trianglePresent <- loadTriangle allocator device queueFamilyIndices shaderRes pipelineRes
  textureShaderRes <- Lib.withTextureShaderStages device
  texturePipelineRes <- Lib.withPipeline device renderPass textureShaderRes
  texturePresent <- loadTexture allocator phys device queueFamilyIndices queueRes commandPoolRes textureShaderRes texturePipelineRes
  -- resource load end
  
  let fps = 1
  let inputfps = 120
  let sync = 0
  
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  swapchainRes@SwapchainResource {..} <- withSwapchain phys device surf surfaceFormat queueFamilyIndices extent renderPass NULL_HANDLE
  let frameSize = fromIntegral . length $ framebuffers
  commandBufferRes@CommandBufferResource {..} <- withCommandBufferResource device graphicsCommandPool frameSize
  mapM_ (submitCommand extent renderPass [ trianglePresent, texturePresent ]) (V.zip commandBuffers framebuffers)

  SyncResource {..} <- withSyncResource device framebuffers
  let frame = Frame {..}
  let ctx = Context {..}

  _ <- liftIO . forkIO . drain . asyncly $
    parallel
      (constRate inputfps $ repeatM $ liftIO $ eventLoop eventQueue)
      (repeatM $ runStdoutLoggingT $ listenLoop eventQueue)
  liftIO . S.drainWhile isJust . S.drop 1 . asyncly . minRate fps . maxRate fps . S.iterateM (maybe (pure Nothing) (runResourceT . drawFrame)) . pure . Just $ (ctx, frame, commandBufferRes, swapchainRes)
  deviceWaitIdleSafe device
  return undefined

data Frame = Frame
  { imageAvailableSemaphores :: V.Vector Semaphore
  , renderFinishedSemaphores :: V.Vector Semaphore
  , submitFinishedFences :: V.Vector Fence
  , frameSize :: Word32
  , sync :: Int
  }

data Context = Context
  { phys :: PhysicalDevice
  , device :: Device
  , graphicsQueue :: Queue
  , presentQueue :: Queue
  , window :: SDL.Window
  , surf :: SurfaceKHR
  , queueFamilyIndices :: V.Vector Word32
  , surfaceFormat :: SurfaceFormatKHR
  , renderPass :: RenderPass
  }

recreateSwapchain :: Managed m => Context -> CommandBufferResource -> SwapchainResource -> m (CommandBufferResource, SwapchainResource)
recreateSwapchain Context {..} CommandBufferResource {..} oldSwapchainRes@SwapchainResource { swapchain } = do
  deviceWaitIdle device
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  liftIO . print $ "recreate width " <> show extent
  
  swapchainRes@SwapchainResource { framebuffers } <- withSwapchain phys device surf surfaceFormat queueFamilyIndices extent renderPass swapchain
  release . swapchainResourceKey $ oldSwapchainRes
  let frameSize = fromIntegral . length $ framebuffers
  commandBufferRes <- withCommandBufferResource device commandPool frameSize
  pure (commandBufferRes, swapchainRes)

drawFrameHandler :: (Managed m) => Context -> Frame -> CommandBufferResource -> SwapchainResource -> VulkanException -> m (Maybe (Context, Frame, CommandBufferResource, SwapchainResource))
drawFrameHandler ctx frame cmdr swpr (VulkanException e@ERROR_OUT_OF_DATE_KHR) = do
  (commandBufferRes, swapchainRes) <- recreateSwapchain ctx cmdr swpr
  pure . Just $ (ctx, frame, commandBufferRes, swapchainRes)
drawFrameHandler _ _ _ _ e = do
  liftIO . print $ "throw " <> show e
  throw e

drawFrame :: (Managed m) => (Context, Frame, CommandBufferResource, SwapchainResource) -> m (Maybe (Context, Frame, CommandBufferResource, SwapchainResource))
drawFrame (ctx@Context {..}, frame@Frame {..}, cmdr@CommandBufferResource {..}, swpr@SwapchainResource {..}) = (fmap liftIO . Ex.handle) (runResourceT . drawFrameHandler ctx frame cmdr swpr) $ do
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  liftIO . print $ "width " <> show extent
  V2 w h <- StateVar.get $ SDL.windowSize window
  liftIO . print $ "window " <> show w <> " " <> show h 
  let commandBuffer = commandBuffers ! sync
  let imageAvailableSemaphore = imageAvailableSemaphores ! sync
  let renderFinishedSemaphore = renderFinishedSemaphores ! sync
  let fence = submitFinishedFences ! sync
  _ <- waitForFencesSafe device [ fence ] True maxBound
  resetFences device [ fence ]
  imageIndex <- snd <$> acquireNextImageKHRSafe device swapchain maxBound imageAvailableSemaphore zero
  queueSubmit graphicsQueue
    [ SomeStruct $ zero
      { Core10.waitSemaphores = [ imageAvailableSemaphore ]
      , waitDstStageMask = [ PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ]
      , commandBuffers = [ commandBufferHandle commandBuffer ]
      , signalSemaphores = [ renderFinishedSemaphore ]
      }
    ] fence
  _ <- queuePresentKHR presentQueue zero
    { Swap.waitSemaphores = [ renderFinishedSemaphore ]
    , swapchains = [ swapchain ]
    , imageIndices = [ imageIndex ]
    }
  -- queueWaitIdle presentQueue
  -- queueWaitIdle graphicsQueue
  pure . Just $
    ( ctx
    , frame
      { sync = (sync + 1) `mod` fromIntegral frameSize
      }
    , cmdr
    , swpr
    )

loadTriangle :: Managed m => Vma.Allocator -> Device -> V.Vector Word32 -> ShaderResource -> PipelineResource -> m Present
loadTriangle allocator device queueFamilyIndices shaderRes pipelineRes = do
  
  (vertexBuffer, vertexBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ 3 * sizeOf (undefined :: ShaderInputVertex)
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT -- support vkCmdBindVertexBuffers.pBuffers
    , sharingMode = SHARING_MODE_EXCLUSIVE -- chooseSharingMode queueFamilyIndices
    -- , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU--GPU_ONLY
    } allocate
  let vertices =
        [ ShaderInputVertex (V2 250 125) (V4 (102/255) (53/255) (53/255) 1)
        , ShaderInputVertex (V2 375 (375)) (V4 (53/255) (102/255) (53/255) 1)
        , ShaderInputVertex (V2 (125) (375)) (V4 (53/255) (53/255) (102/255) 1)
        ] :: VS.Vector ShaderInputVertex
  liftIO . runResourceT $ memCopy allocator vertexBufferAllocation vertices -- early free

  let indices = [0, 1, 2] :: VS.Vector Word32
  (indexBuffer, indexBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (indices VS.! 0) * VS.length indices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  liftIO . runResourceT $ memCopy allocator indexBufferAllocation indices

  (uniformBuffer, uniformBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ 1 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  let uniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (500) (0) (500) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (500/2*0.5) (500/2*0.5) 0) (axisAngle (V3 0 0 1) (45/360*2*pi)) !*! (m33_to_m44 . scaled $ 0.5)
        }
  liftIO . runResourceT $ memCopyU allocator uniformBufferAllocation uniform -- early free
  liftIO . print $ descriptorSetLayoutCreateInfos shaderRes
  descriptorSetResource <- withDescriptorSetResource device (descriptorSetLayouts shaderRes) (descriptorSetLayoutCreateInfos shaderRes)
  liftIO . print $ descriptorSetResource
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
      { dstSet = descriptorSets (descriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 0
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = fromIntegral . length $ bufferInfos
      , bufferInfo = bufferInfos
      , imageInfo = []
      , texelBufferView = []
      }
    ] []
  pure $ Present [vertexBuffer] indexBuffer (descriptorSets (descriptorSetResource :: DescriptorSetResource)) (fromIntegral . VS.length $ indices) pipelineRes

loadTexture :: Managed m => Vma.Allocator -> PhysicalDevice -> Device -> V.Vector Word32 -> QueueResource -> CommandPoolResource  -> ShaderResource -> PipelineResource -> m Present
loadTexture allocator phys device queueFamilyIndices QueueResource {..} CommandPoolResource {..}  textureShaderRes pipelineRes = do
  liftIO . print . descriptorSetLayouts $ textureShaderRes
  textureSampler <- withTextureSampler phys device
  textureDescriptorSetResource <- withDescriptorSetResource device (descriptorSetLayouts textureShaderRes) (descriptorSetLayoutCreateInfos textureShaderRes)
  let texCoords =
        [ Texture (V2 50 50) (V4 0 0 0 1) (V2 0 0)
        , Texture (V2 250 50) (V4 0 0 0 1) (V2 1 0)
        , Texture (V2 250 150) (V4 0 0 0 1) (V2 1 1)
        , Texture (V2 50 150) (V4 0 0 0 1) (V2 0 1)
        ] :: VS.Vector Texture
  (texCoordsBuffer, texCoordsBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texCoords VS.! 0) * VS.length texCoords
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  liftIO . runResourceT $ memCopy allocator texCoordsBufferAllocation texCoords

  let texIndices = [0, 1, 2, 2, 3, 0] :: VS.Vector Word32
  (texIndexBuffer, texIndexBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texIndices VS.! 0) * VS.length texIndices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  liftIO . runResourceT $ memCopy allocator texIndexBufferAllocation texIndices
  
  let texUniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (500) (0) (500) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (150/2*1) (100/2*1) 0) (axisAngle (V3 0 0 1) (45/360*2*pi)) !*! (m33_to_m44 . scaled $ 1)
        }
  (texUniformBuffer, texUniformBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ 4 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } allocate
  liftIO . runResourceT $ memCopy allocator texUniformBufferAllocation (VS.fromList [texUniform, texUniform, texUniform, texUniform]) -- early free
  let texBufferInfos :: V.Vector DescriptorBufferInfo
      texBufferInfos =
        [ zero
          { buffer = texUniformBuffer
          , offset = 0
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*1) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*2) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        , zero
          { buffer = texUniformBuffer
          , offset = fromIntegral . (*3) . sizeOf $ (undefined :: ShaderUniform)
          , range = fromIntegral . sizeOf $ (undefined :: ShaderUniform)
          } :: DescriptorBufferInfo
        ]

  let pixels = renderDrawing 200 100 (PixelRGBA8 255 255 0 100) $ fill $ rectangle (V2 0 0) 200 100
  (textureStagingBuffer, textureStagingBufferAllocation, _) <- snd <$> Vma.withBuffer allocator zero
    { size = fromIntegral $ (sizeOf . VS.head $ imageData pixels) * VS.length (imageData pixels)
    , usage = BUFFER_USAGE_TRANSFER_SRC_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_ONLY
    } allocate
  liftIO . runResourceT $ memCopy allocator textureStagingBufferAllocation (imageData pixels)
  let textureFormat = FORMAT_R8G8B8A8_SRGB
  (textureImage, textureImageAllocation, _) <- snd <$> Vma.withImage allocator zero
    { imageType = IMAGE_TYPE_2D
    , extent = Extent3D 200 100 1
    , mipLevels = 1
    , arrayLayers = 1
    , format = textureFormat
    , tiling = IMAGE_TILING_OPTIMAL
    , initialLayout = IMAGE_LAYOUT_UNDEFINED
    , usage = IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT -- when use staging buffer, VK_IMAGE_USAGE_TRANSFER_DST_BIT is necessary. VUID-VkImageMemoryBarrier-oldLayout-01213
    , samples = SAMPLE_COUNT_1_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_GPU_ONLY
    } allocate

  withSingleTimeCommands device transferCommandPool transferQueue $ \cb -> do
    transitionImageLayout cb textureImage IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    cmdCopyBufferToImage cb textureStagingBuffer textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      [ zero
        { bufferOffset = 0
        , bufferRowLength = 0
        , bufferImageHeight = 0
        , imageSubresource =  zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , mipLevel = 0
          , baseArrayLayer = 0
          , layerCount = 1
          }
        , imageOffset = Offset3D 0 0 0
        , imageExtent = Extent3D (fromIntegral . JP.imageWidth $ pixels) (fromIntegral . JP.imageHeight $ pixels) 1
        } :: BufferImageCopy
      ]
    transitionImageLayout cb textureImage IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  (_imageViewKey, textureImageView) <- Lib.withImageView device textureFormat textureImage
  updateDescriptorSets device
    [ SomeStruct $ zero
      { dstSet = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 0
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = fromIntegral . length $ texBufferInfos
      , bufferInfo = texBufferInfos
      , imageInfo = []
      , texelBufferView = []
      }
    , SomeStruct $ zero
      { dstSet = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource) ! 2
      , dstBinding = 1
      , dstArrayElement = 0
      , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
      , descriptorCount = 1
      , imageInfo =
        [ zero
          { imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          , imageView = textureImageView
          , sampler = textureSampler
          }
        ]
      }
    ] []
  pure $ Present [texCoordsBuffer] texIndexBuffer (descriptorSets (textureDescriptorSetResource :: DescriptorSetResource)) (fromIntegral . VS.length $ texIndices) pipelineRes

type Managed m = MonadResource m

data AppException
  = ImageLoadException String
  | VulkanDeviceNotFound
  | VulkanAllocateMemoryException String
  | VulkanGraphicsFamilyIndexException
  | VulkanPresentFamilyIndexException
  | VulkanTransferFamilyIndexException
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

withSDL :: Managed m => m ()
withSDL = do
  _sdlInitKey <- allocate_ (SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])) SDL.quit
  _sdlVkKey <- allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  pure ()

withWindow :: Managed m => Text -> Int -> Int -> m SDL.Window
withWindow title width height = do
  (_key, window) <- allocate (SDL.createWindow title SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height)
    , SDL.windowGraphicsContext = SDL.VulkanContext
    , SDL.windowResizable = True--False
    , SDL.windowMode = SDL.Windowed
    , SDL.windowPosition = SDL.Centered
    , SDL.windowBorder = False--True
    })
    SDL.destroyWindow
  SDL.windowMinimumSize window SDL.$= SDL.V2 (fromIntegral width) (fromIntegral height)
  pure window

withInst :: Managed m => SDL.Window -> m Instance
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

withSurface :: Managed m => Instance -> SDL.Window -> m SurfaceKHR
withSurface inst window = do
  (_key, surf) <- allocate (SurfaceKHR <$> SDL.vkCreateSurface window (castPtr . instanceHandle $ inst))
    (flip (destroySurfaceKHR inst) Nothing)
  pure surf

getPhysicalDevice :: MonadIO m => Instance -> m PhysicalDevice
getPhysicalDevice inst = do
  (_, devices) <- enumeratePhysicalDevices inst
  if null devices
    then throw VulkanDeviceNotFound
    else pure $ devices ! 0

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
  let graphicsFamily = tryWithE VulkanGraphicsFamilyIndexException $ graphicsQueueFamilyIndices !? 0
  let presentFamily = tryWithE VulkanPresentFamilyIndexException $ pickPresentFamilyIndices [graphicsFamily] presentQueueFamilyIndices !? 0
  let transferFamily = if not . null $ transferOnlyQueueFamilyIndices
        then transferOnlyQueueFamilyIndices ! 0
        else tryWithE VulkanPresentFamilyIndexException (pickTransferFamilyIndices [graphicsFamily] [presentFamily] transferQueueFamilyIndices !? 0)
  pure QueueFamilyIndices
    { graphicsFamily = fromIntegral graphicsFamily
    , presentFamily = fromIntegral presentFamily
    , transferFamily = fromIntegral transferFamily
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

withDevice :: Managed m => PhysicalDevice -> "queueFamilyIndices" ::: V.Vector Word32 -> m Device
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

data QueueResource = QueueResource
  { graphicsQueue :: !Queue
  , presentQueue :: !Queue
  , transferQueue :: !Queue
  } deriving (Show)

getQueueResource :: MonadIO m => Device -> QueueFamilyIndices -> m QueueResource
getQueueResource device QueueFamilyIndices {..} = do
  graphicsQueue <- getDeviceQueue device graphicsFamily 0
  presentQueue <- getDeviceQueue device presentFamily 0
  transferQueue <- getDeviceQueue device transferFamily 0
  pure QueueResource {..}

data CommandPoolResource = CommandPoolResource
  { graphicsCommandPool :: CommandPool
  , transferCommandPool :: CommandPool
  } deriving (Show)

withCommandPoolResource :: Managed m => Device -> QueueFamilyIndices -> m CommandPoolResource
withCommandPoolResource device QueueFamilyIndices {..} = do
  graphicsCommandPool <- snd <$> withCommandPool device zero
    { queueFamilyIndex = graphicsFamily
    , flags = zeroBits
    } Nothing allocate
  transferCommandPool <- snd <$> withCommandPool device zero
    { queueFamilyIndex = transferFamily
    , flags = COMMAND_POOL_CREATE_TRANSIENT_BIT
    } Nothing allocate
  pure CommandPoolResource {..}

withSingleTimeCommands :: MonadIO m => Device -> CommandPool -> Queue -> (CommandBuffer -> IO ()) -> m ()
withSingleTimeCommands device commandPool queue f = liftIO . runResourceT $ do
  commandBuffer <- V.head . snd <$> Vulkan.withCommandBuffers device zero
    { commandPool = commandPool
    , level = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = 1
    } allocate
  liftIO $ useCommandBuffer commandBuffer zero
    { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } (f commandBuffer)
  fence <- snd <$> withFence device zero Nothing allocate
  queueSubmit queue
    [ SomeStruct (zero
      { commandBuffers = [ commandBufferHandle commandBuffer ]
      } :: SubmitInfo '[])
    ] fence
  _ <- waitForFences device [ fence ] True maxBound
  pure ()

data SwapchainResource = SwapchainResource
  { swapchain :: !SwapchainKHR
  , images :: !(V.Vector Image)
  , imageViews :: !(V.Vector ImageView)
  , framebuffers :: !(V.Vector Framebuffer)
  , swapchainResourceKey :: ReleaseKey
  }

chooseSharingMode :: "queueFamilyIndices" ::: V.Vector Word32 -> SharingMode
chooseSharingMode indices | length indices == 1 = SHARING_MODE_EXCLUSIVE
                          | otherwise = SHARING_MODE_CONCURRENT

chooseFormat :: V.Vector SurfaceFormatKHR -> SurfaceFormatKHR
chooseFormat formats = fromMaybe (V.head formats) $ V.find isSRGB formats
  where
    isSRGB = (==) SurfaceFormatKHR {format = FORMAT_B8G8R8A8_SRGB, colorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR}

withSwapchain :: Managed m => PhysicalDevice -> Device -> SurfaceKHR -> SurfaceFormatKHR -> "queueFamilyIndices" ::: V.Vector Word32 -> Extent2D -> RenderPass -> SwapchainKHR -> m SwapchainResource
withSwapchain phys device surf surfaceFormat indices extent renderPass oldSwapchain = do
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
  (swapchainKey, swapchain) <- withSwapchainKHR device swapchainCreateInfo Nothing allocate
  images <- snd <$> getSwapchainImagesKHR device swapchain
  (imageViewKeys, imageViews) <- fmap V.unzip . mapM (Lib.withImageView device (format (surfaceFormat :: SurfaceFormatKHR))) $ images
  (framebufferKeys, framebuffers) <- fmap V.unzip . mapM (Lib.withFramebuffer device extent renderPass) $ imageViews
  releaseSwapchain <- unprotect swapchainKey
  releaseImageViewKeys <- mapM_ unprotect imageViewKeys
  releaseFramebufferKeys <- mapM_ unprotect framebufferKeys 
  swapchainResourceKey <- register $ do
    -- release swapchainKey
    -- mapM_ release imageViewKeys
    -- mapM_ release framebufferKeys
    pure ()
  pure SwapchainResource {..}

withImageView :: Managed m => Device -> Format -> Image -> m (ReleaseKey, ImageView)
withImageView device format img = Vulkan.withImageView device zero
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
  } Nothing allocate

data ShaderResource = ShaderResource
  { shaderStages :: !(V.Vector (SomeStruct PipelineShaderStageCreateInfo))
  , descriptorSetLayouts :: !(V.Vector DescriptorSetLayout)
  , descriptorSetLayoutCreateInfos :: !(V.Vector (DescriptorSetLayoutCreateInfo '[]))
  , vertexInputState :: !(Maybe (SomeStruct PipelineVertexInputStateCreateInfo))
  }
  deriving (Show)

withShaderModule :: Managed m => Device -> SpirV.ShaderInfo -> m ShaderModule
withShaderModule device SpirV.ShaderInfo {..} = snd <$> Vulkan.withShaderModule device shaderModuleCreateInfo Nothing allocate

withShaders :: Managed m => Device -> V.Vector ("spirv" ::: ByteString) -> m ShaderResource
withShaders device spirvs = do
  -- the reflection should be done in compile stage
  reflects <- V.mapM SpirV.reflection' spirvs
  let shaderInfos = SpirV.makeShaderInfo <$> reflects
  shaderStages <- (join <$>) . V.mapM (liftA2 (<$>) SpirV.pipelineShaderStageCreateInfos (Lib.withShaderModule device)) $ shaderInfos
  let descriptorSetLayoutCreateInfos = SpirV.makeDescriptorInfo reflects
  descriptorSetLayouts <- mapM (Lib.withDescriptorSetLayout device) descriptorSetLayoutCreateInfos
  let vertexInputState = SpirV.makeInputInfo reflects
  pure ShaderResource {..}

withShaderStages :: Managed m => Device -> m ShaderResource
withShaderStages device = do
  let vertCode = [vert|
  #version 450
  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo;

  layout(location = 0) in vec2 inPosition;
  layout(location = 1) in vec4 inColor;

  layout(location = 0) out vec4 fragColor;

  void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
    fragColor = inColor;
  }

  |]
  let fragCode = [frag|
  #version 450

  #extension GL_ARB_separate_shader_objects : enable

  layout(location = 0) in vec4 fragColor;

  layout(location = 0) out vec4 outColor; 

  void main() {
    outColor = fragColor;
  }
  |]
  withShaders device [ vertCode, fragCode ]

withTextureShaderStages :: Managed m => Device -> m ShaderResource
withTextureShaderStages device = do
  let vertCode = [vert|
  #version 450
  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo[4];

  layout(location = 0) in vec2 position;
  layout(location = 1) in vec4 color;
  layout(location = 2) in vec2 texCoord;

  layout(location = 0) out vec4 fragColor;
  layout(location = 1) out vec2 fragTexCoord;

  void main() {
    gl_Position = ubo[3].proj * ubo[3].view * ubo[3].model * vec4(position, 0.0, 1.0);
    fragColor = color;
    fragTexCoord = texCoord;
  }
  |]
  let fragCode = [frag|
  #version 450

  #extension GL_ARB_separate_shader_objects : enable

  layout(set = 2, binding = 1) uniform sampler2D texSampler;

  layout(location = 0) in vec4 fragColor;
  layout(location = 1) in vec2 fragTexCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler, fragTexCoord); // for image use texSampler, for shape created by rasterfic use fragColor
  }
  |]
  withShaders device [ vertCode, fragCode ]

withDescriptorSetLayout :: Managed m => Device -> DescriptorSetLayoutCreateInfo '[] -> m DescriptorSetLayout
withDescriptorSetLayout device descriptorSetLayoutCreateInfo =
  snd <$> Vulkan.withDescriptorSetLayout device descriptorSetLayoutCreateInfo Nothing allocate

data DescriptorSetResource = DescriptorSetResource
  { descriptorPool :: !DescriptorPool
  , descriptorSets :: !(V.Vector DescriptorSet)
  } deriving (Show)

withDescriptorSetResource :: Managed m => Device -> V.Vector DescriptorSetLayout -> V.Vector (DescriptorSetLayoutCreateInfo '[]) -> m DescriptorSetResource
withDescriptorSetResource device descriptorSetLayouts descriptorSetLayoutCreateInfos = do
  -- https://www.reddit.com/r/vulkan/comments/8u9zqr/having_trouble_understanding_descriptor_pool/e1e8d5f?utm_source=share&utm_medium=web2x&context=3
  -- https://www.reddit.com/r/vulkan/comments/clffjm/descriptorpool_maxsets_how_does_this_work_if_you/
  -- https://www.reddit.com/r/vulkan/comments/aij7zp/there_is_a_good_technique_to_update_a_vertex/
  let descriptorPoolCreateInfo = makeDescriptorPoolCreateInfo (fromIntegral . length $ descriptorSetLayouts) descriptorSetLayoutCreateInfos
  descriptorPool <- snd <$> withDescriptorPool device descriptorPoolCreateInfo Nothing allocate
  descriptorSets <- snd <$> withDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts = descriptorSetLayouts
    } allocate
  pure DescriptorSetResource {..}

makeDescriptorPoolCreateInfo :: Word32 -> V.Vector (DescriptorSetLayoutCreateInfo '[]) -> DescriptorPoolCreateInfo '[]
makeDescriptorPoolCreateInfo maxSets infos = zero
  { poolSizes = V.fromList
    [ zero
      { type' = t
      , descriptorCount = fromIntegral n
      }
    | (t, n) <- analyse infos ]
  , maxSets = maxSets
  , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT -- VUID-vkFreeDescriptorSets-descriptorPool-00312: descriptorPool must have been created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag
  }
  where
    analyse :: V.Vector (DescriptorSetLayoutCreateInfo '[]) -> [(DescriptorType, Int)]
    analyse = map calculate . groupBy ((==) `on` fst) . sortOn fst . extract
    extract :: V.Vector (DescriptorSetLayoutCreateInfo '[]) -> [(DescriptorType, Int)]
    extract = map (liftA2 (,) tname (fromIntegral . dcount)) . V.toList . (bindings =<<)
    calculate :: [(DescriptorType, Int)] -> (DescriptorType, Int)
    calculate = liftA2 (,) (fst . head) (sum . (snd <$>))
    tname = descriptorType :: DescriptorSetLayoutBinding -> DescriptorType
    dcount = descriptorCount :: DescriptorSetLayoutBinding -> Word32

withRenderPass :: Managed m => Device -> SurfaceFormatKHR -> m RenderPass
withRenderPass device surfFormat = do
  let colorAttachment = zero
        { format = format (surfFormat :: SurfaceFormatKHR)
        , samples = SAMPLE_COUNT_1_BIT
        , loadOp = ATTACHMENT_LOAD_OP_CLEAR
        , storeOp = ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
        } :: AttachmentDescription
  let colorAttachmentRef = zero
        { attachment = 0
        , layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        } :: AttachmentReference
  let subpass = zero
        { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
        , colorAttachments = [ colorAttachmentRef ]
        } :: SubpassDescription
  snd <$> Vulkan.withRenderPass device zero
    { attachments = [ colorAttachment ]
    , subpasses = [ subpass ]
    } Nothing allocate

withFramebuffer :: Managed m => Device -> Extent2D -> RenderPass -> ImageView -> m (ReleaseKey, Framebuffer)
withFramebuffer device curExtent renderPass imageView = Vulkan.withFramebuffer device zero
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

withPipeline :: Managed m => Device -> RenderPass -> ShaderResource -> m PipelineResource
withPipeline device renderPass ShaderResource {..} = do
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
            , blendEnable = True
            , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
            , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
            , colorBlendOp = BLEND_OP_ADD
            , srcAlphaBlendFactor = BLEND_FACTOR_ONE
            , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
            , alphaBlendOp = BLEND_OP_ADD
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
  let pipeline = pipelines ! 0
  pure PipelineResource {..}

withTextureSampler :: Managed m => PhysicalDevice -> Device -> m Sampler
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

transitionImageLayout :: MonadIO m => CommandBuffer -> Image -> "oldLayout" ::: ImageLayout -> "newLayout" ::: ImageLayout -> m ()
transitionImageLayout commandBuffer image oldLayout newLayout = do
  let barrier = zero
        { oldLayout = oldLayout
        , newLayout = newLayout
        , image = image
        , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
        , subresourceRange = zero
          { aspectMask = IMAGE_ASPECT_COLOR_BIT
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
        }
  case (oldLayout, newLayout) of
    (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
      cmdPipelineBarrier commandBuffer PIPELINE_STAGE_TOP_OF_PIPE_BIT PIPELINE_STAGE_TRANSFER_BIT zero [] []
      [ SomeStruct (barrier
        { srcAccessMask = zero
        , dstAccessMask = ACCESS_TRANSFER_WRITE_BIT
        } :: ImageMemoryBarrier '[]) ]
    (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
      cmdPipelineBarrier commandBuffer PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero [] []
      [ SomeStruct (barrier
        { srcAccessMask = ACCESS_TRANSFER_WRITE_BIT
        , dstAccessMask = ACCESS_SHADER_READ_BIT
        } :: ImageMemoryBarrier '[]) ]
    _ -> throw VulkanLayoutTransitionUnsupport

data CommandBufferResource = CommandBufferResource
  { commandPool :: CommandPool
  , commandBuffers :: V.Vector CommandBuffer
  }

withCommandBuffers :: Managed m => Device -> CommandPool -> Word32 -> m (V.Vector CommandBuffer)
withCommandBuffers device commandPool frameSize =
  snd <$> Vulkan.withCommandBuffers device zero
    { commandPool = commandPool
    , level = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = frameSize
    } allocate

withCommandBufferResource :: Managed m => Device -> CommandPool -> Word32 -> m CommandBufferResource
withCommandBufferResource device commandPool frameSize = do
  commandBuffers <- Lib.withCommandBuffers device commandPool frameSize
  liftIO $ print $ V.map commandBufferHandle commandBuffers
  pure $ CommandBufferResource
    { commandPool = commandPool
    , commandBuffers = commandBuffers
    }

data Present = Present
  { vertexBuffers :: !(V.Vector Buffer)
  , indexBuffer :: !Buffer
  , descriptorSets :: !(V.Vector DescriptorSet)
  , drawSize :: !Word32
  , pipelineResource :: !PipelineResource
  }

submitCommand :: Managed m
              => "renderArea" ::: Extent2D
              -> RenderPass
              -> V.Vector Present
              -> (CommandBuffer, Framebuffer)
              -> m ()
submitCommand extent@Extent2D {..} renderPass presents (commandBuffer, framebuffer) = do
  let viewports =
        [ Viewport
          { x = 0
          , y = 0
          , width = fromIntegral width
          , height = fromIntegral height
          , minDepth = 0
          , maxDepth = 1
          }
        ] :: V.Vector Viewport
  let scissors =
        [ Rect2D
          { offset = Offset2D 0 0
          , extent = extent
          }
        ] :: V.Vector Rect2D
  liftIO $ useCommandBuffer commandBuffer zero -- do
    { flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT --COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    } $ cmdUseRenderPass commandBuffer zero
      { renderPass = renderPass
      , framebuffer = framebuffer
      , renderArea = Rect2D
        { offset = zero
        , extent = extent
        }
      , clearValues = [ Color $ Float32 1 1 1 1 ]
      } SUBPASS_CONTENTS_INLINE $ mapM_ (presentCmd viewports scissors) presents
  where
    presentCmd :: V.Vector Viewport -> V.Vector Rect2D -> Present -> IO ()
    presentCmd viewports scissors Present {..} = do
      cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS ((pipeline :: PipelineResource -> Pipeline) pipelineResource)
      let offsets = const 0 <$> vertexBuffers
      cmdSetViewport commandBuffer 0 viewports
      cmdSetScissor commandBuffer 0 scissors
      cmdBindVertexBuffers commandBuffer 0 vertexBuffers offsets
      cmdBindIndexBuffer commandBuffer indexBuffer 0 INDEX_TYPE_UINT32
      cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_GRAPHICS ((pipelineLayout :: PipelineResource -> PipelineLayout) pipelineResource) 0 descriptorSets []
      cmdDrawIndexed commandBuffer drawSize 1 0 0 0
      --cmdDraw commandBuffer drawSize 1 0 0

data SyncResource = SyncResource
  { imageAvailableSemaphores :: !(V.Vector Semaphore)
  , renderFinishedSemaphores :: !(V.Vector Semaphore)
  , submitFinishedFences :: !(V.Vector Fence)
  }

withSyncResource :: Managed m => Device -> V.Vector Framebuffer -> m SyncResource
withSyncResource device framebuffers = do
  imageAvailableSemaphores <- mapM (const makeSemaphore) framebuffers
  renderFinishedSemaphores <- mapM (const makeSemaphore) framebuffers
  submitFinishedFences <- mapM (const makeFence) framebuffers
  pure $ SyncResource {..}
  where
    makeSemaphore = snd <$> withSemaphore device zero Nothing allocate
    makeFence = snd <$> withFence device zero { flags = FENCE_CREATE_SIGNALED_BIT } Nothing allocate

memCopy :: forall a m . (Storable a, Managed m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> VS.Vector a -> m ()
memCopy allocator memAllocation datas = do
  bufferMemoryPtr <- snd <$> Vma.withMappedMemory allocator memAllocation allocate
  liftIO $ VS.unsafeWith datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral (sizeOf (undefined :: a)) * VS.length datas

memCopyU :: forall a m . (Storable a, Managed m) => Vma.Allocator -> "deviceMemory" ::: Vma.Allocation -> a -> m ()
memCopyU allocator memAllocation datas = do
  bufferMemoryPtr <- snd <$> Vma.withMappedMemory allocator memAllocation allocate
  liftIO $ with datas $ \ptr ->
    copyBytes bufferMemoryPtr (castPtr ptr) $ fromIntegral . sizeOf $ (undefined :: a)

