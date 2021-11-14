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
    , testfont
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
import Codec.Picture (PixelRGBA8 (..), readImage, imageData)
import qualified Codec.Picture as JP
import Graphics.Rasterific (renderDrawing, rectangle, fill)
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Text.TrueType (loadFontFile, decodeFont, descriptorOf)

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
--import Data.Acquire (Acquire, mkAcquire)
import Data.String (IsString)
import Data.Word (Word32)
import Data.Text (Text)
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
import Numeric (showHex)

import Streamly
import Streamly.Prelude (drain, yield, repeatM)
import qualified Streamly.Prelude as S
import Reflex
import qualified Reflex as R
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Reflex.Host.Class (MonadReflexHost, runHostFrame, fireEventsAndRead, fireEvents)
import Reflex.Workflow (Workflow (..))
import Reflex.Network (networkHold)
import Lifetimes (Acquire (..), mkAcquire, withAcquire, releaseEarly, detach)
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
import Control.Monad.Fix (MonadFix)
import Control.Exception (Exception (..), SomeException (..), AsyncException (UserInterrupt), throw, handleJust, try, catch)
import qualified Control.Exception as Ex
import Control.Concurrent (MVar, newMVar, readMVar, forkIO, forkOS, threadDelay, myThreadId)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Data.Functor.Identity (runIdentity)

import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (ThrowC (..), runThrow)
import Control.Carrier.Error.Either (ErrorC (..), runError)
import Control.Effect.Lift (Lift (..))
import Control.Effect.Writer (Writer (..), tell, run)
import Control.Effect.Error (Error)
import Control.Effect.Throw (Throw (..), throwError, liftEither)

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
import Elephantom.Renderer
import qualified Elephantom.Renderer as Lib
import Elephantom.Application (Application (..), defaultApplication)
import Acquire (Acquire, Cleaner, MonadCleaner, mkAcquire, acquireT, cleanup, collect)

import Elephantom.Renderer.RendererException

import System.Environment (getExecutablePath)
import Paths_elephantom (getDataFileName)

testEff :: IO (Either RendererException ())
testEff = runError $ testThrowException

testThrowException :: ( Has (Error RendererException :+: Lift IO) sig m
                      , Has (Lift IO) sig m
                      , MonadIO m
                      ) => m ()
testThrowException = do
  liftIO $ print "hi1"
  sendIO $ print "hi2"
  sendM $ print "hi3"
  x <- throwError VulkanDeviceNotFound
  return ()

testfont :: IO ()
testfont = do
  ep <- getExecutablePath
  print ep
  fontfile <- getDataFileName "font/SourceCodePro-Regular.ttf" -- "font/NotoSansMonoCJKsc-Regular.otf"
  print fontfile
  res <- loadFontFile fontfile
  case res of
    Left e -> print $ "fail to load font " <> e
    Right font -> print $ ("load font " <>) . show <$> descriptorOf font
  print "test font"

-- decodeFont "foo测aefasuqqqqqqqqqqqqqqqqqqqqqqqqx试"

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
  let application@Application {..} = defaultApplication { width = 640, height = 480 } :: Application

  eventQueue <- liftIO newQueue
  withSDL
  window <- snd <$> withWindow "test" width height allocate
  _ <- liftIO . forkIO . drain . asyncly $
    parallel
      (constRate (fromIntegral inputRate) . repeatM . liftIO . eventLoop $ eventQueue)
      (repeatM . runStdoutLoggingT . listenLoop $ eventQueue)
  inst <- withInst application window
  surf <- snd <$> withSurface inst window allocate
  (vendorID, phys) <- getPhysicalDevice inst
  liftIO . print . maxBoundDescriptorSets . limits =<< getPhysicalDeviceProperties phys
  liftIO . print $ ("vendorID: 0x" <> showHex vendorID "")
  qIndices <- findQueueFamilyIndices phys surf
  liftIO $ print qIndices
  let queueFamilyIndices = uniq . modify V.sort $ fmap ($ qIndices) [ graphicsFamily, presentFamily, transferFamily ]
  device <- Lib.withDevice phys queueFamilyIndices
  liftIO $ print $ "device " <> show (deviceHandle device)
  queueRes@QueueResource {..} <- getQueueResource device qIndices
  liftIO $ print $ queueHandle graphicsQueue
  liftIO $ print $ queueHandle presentQueue
  liftIO $ print $ queueHandle transferQueue
  commandPoolRes@CommandPoolResource {..} <- snd <$> withCommandPoolResource device qIndices allocate
  formats <- snd <$> getPhysicalDeviceSurfaceFormatsKHR phys surf
  let surfaceFormat = chooseFormat formats
  liftIO $ print formats
  liftIO $ print surfaceFormat
  renderPass <- snd <$> Lib.withRenderPass device surfaceFormat allocate

  liftIO . print =<< getPhysicalDeviceSurfaceCapabilitiesKHR phys surf

  -- resource load
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/vk__mem__alloc_8h.html#a4f87c9100d154a65a4ad495f7763cf7c
  allocator <- snd <$> Lib.withAllocator phys device inst allocate

  (shaderRes, shaderModules) <- withShaderStages device
  liftIO . print $ shaderModules
  pipelineRes <- snd <$> Lib.withPipelineResource device renderPass shaderRes allocate
  mapM_ (Lib.destroyShaderModule device) shaderModules
  (triangleCleaner, trianglePresent) <- liftIO . collect $ loadTriangle application allocator device queueFamilyIndices shaderRes pipelineRes
  Lib.destroyShaderResource device shaderRes
  (textureShaderRes, textureShaderModules) <- Lib.withTextureShaderStages device
  texturePipelineRes <- snd <$> Lib.withPipelineResource device renderPass textureShaderRes allocate
  liftIO . print $ textureShaderModules
  mapM_ (Lib.destroyShaderModule device) textureShaderModules
  (textureCleaner, texturePresent) <- liftIO . collect $ loadTexture application allocator phys device queueFamilyIndices queueRes commandPoolRes textureShaderRes texturePipelineRes
  Lib.destroyShaderResource device textureShaderRes
  -- resource load end
  
  V2 windowWidth windowHeight <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral windowWidth) (fromIntegral windowHeight)
  swapchainRes@SwapchainResource {..} <- createSwapchain phys device surf surfaceFormat queueFamilyIndices extent renderPass NULL_HANDLE
  
  -- mapM_ (submitCommand extent renderPass [ trianglePresent, texturePresent ]) (V.zip commandBuffers framebuffers)
  presentsMVar <- liftIO . newMVar $ [ trianglePresent, texturePresent ]
  frameSync@FrameSync {..} <- createFrameSync device framebuffers

  commandBufferRes@CommandBufferResource {..} <- createCommandBufferResource device graphicsCommandPool frameSize
  cleanupMVar <- liftIO . newMVar $ (frameSync, commandBufferRes, swapchainRes)
  let ctx = Context {..}

  liftIO . flip Ex.finally (cleanupFrame device cleanupMVar >> foldMap cleanup ([ triangleCleaner, textureCleaner ] :: [ Cleaner ])) . S.drainWhile isJust . S.drop 1 . asyncly . minRate (fromIntegral fps) . maxRate (fromIntegral fps) . S.iterateM (maybe (pure Nothing) drawFrame) . pure . Just $ (ctx, frameSync, commandBufferRes, swapchainRes)

loadTriangle :: MonadCleaner m => Application -> Vma.Allocator -> Device -> V.Vector Word32 -> ShaderResource -> PipelineResource -> m Present
loadTriangle Application { width, height } allocator device queueFamilyIndices shaderRes pipelineRes = do

  (vertexBuffer, vertexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 3 * sizeOf (undefined :: ShaderInputVertex)
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT -- support vkCmdBindVertexBuffers.pBuffers
    , sharingMode = SHARING_MODE_EXCLUSIVE -- chooseSharingMode queueFamilyIndices
    -- , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU--GPU_ONLY
    } acquireT
  let vertices =
        [ ShaderInputVertex (V2 250 125) (V4 (102/255) (53/255) (53/255) 1)
        , ShaderInputVertex (V2 375 (375)) (V4 (53/255) (102/255) (53/255) 1)
        , ShaderInputVertex (V2 (125) (375)) (V4 (53/255) (53/255) (102/255) 1)
        ] :: VS.Vector ShaderInputVertex
  memCopy allocator vertexBufferAllocation vertices -- early free

  let indices = [0, 1, 2] :: VS.Vector Word32
  (indexBuffer, indexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (indices VS.! 0) * VS.length indices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopy allocator indexBufferAllocation indices

  (uniformBuffer, uniformBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 1 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  let uniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (fromIntegral width) (0) (fromIntegral height) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (500/2*0.5) (500/2*0.5) 0) (axisAngle (V3 0 0 1) (45/360*2*pi)) !*! (m33_to_m44 . scaled $ 0.5)
        }
  memCopyU allocator uniformBufferAllocation uniform -- early free
  liftIO . print $ descriptorSetLayoutCreateInfos shaderRes
  descriptorSetResource <- Lib.withDescriptorSetResource device (descriptorSetLayouts shaderRes) (descriptorSetLayoutCreateInfos shaderRes) acquireT
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
  pure Present
    { vertexBuffers = [ vertexBuffer ]
    , indexBuffer = indexBuffer
    , descriptorSets = descriptorSets (descriptorSetResource :: DescriptorSetResource)
    , drawSize = fromIntegral . VS.length $ indices
    , pipelineResource = pipelineRes
    }

loadTexture :: (MonadIO m, MonadCleaner m) => Application -> Vma.Allocator -> PhysicalDevice -> Device -> V.Vector Word32 -> QueueResource -> CommandPoolResource -> ShaderResource -> PipelineResource -> m Present
loadTexture Application { width, height } allocator phys device queueFamilyIndices QueueResource {..} CommandPoolResource {..}  textureShaderRes pipelineRes = do
  liftIO . print . descriptorSetLayouts $ textureShaderRes
  textureSampler <- Lib.withTextureSampler phys device acquireT
  textureDescriptorSetResource <- Lib.withDescriptorSetResource device (descriptorSetLayouts textureShaderRes) (descriptorSetLayoutCreateInfos textureShaderRes) acquireT
  let texCoords =
        [ Texture (V2 50 50) (V4 0 0 0 1) (V2 0 0)
        , Texture (V2 250 50) (V4 0 0 0 1) (V2 1 0)
        , Texture (V2 250 150) (V4 0 0 0 1) (V2 1 1)
        , Texture (V2 50 150) (V4 0 0 0 1) (V2 0 1)
        ] :: VS.Vector Texture
  (texCoordsBuffer, texCoordsBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texCoords VS.! 0) * VS.length texCoords
    , usage = BUFFER_USAGE_VERTEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopy allocator texCoordsBufferAllocation texCoords

  let texIndices = [0, 1, 2, 2, 3, 0] :: VS.Vector Word32
  (texIndexBuffer, texIndexBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ sizeOf (texIndices VS.! 0) * VS.length texIndices
    , usage = BUFFER_USAGE_INDEX_BUFFER_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero {
      Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopy allocator texIndexBufferAllocation texIndices
  
  let texUniform = ShaderUniform
        { view = identity -- lookAt 0 0 (V3 0 0 (-1)) -- for 2D UI, no need for a view martix
        , proj = transpose $ ortho (0) (fromIntegral width) (0) (fromIntegral height) (fromIntegral (-maxBound::Int)) (fromIntegral (maxBound::Int))
        , model = transpose $ mkTransformation (axisAngle (V3 0 0 1) (0)) (V3 0 0 0) !*! rotateAt (V3 (150/2*1) (100/2*1) 0) (axisAngle (V3 0 0 1) ((45+30)/360*2*pi)) !*! (m33_to_m44 . scaled $ 1)
        }
  (texUniformBuffer, texUniformBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ 4 * sizeOf (undefined :: ShaderUniform)
    , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT -- .|. BUFFER_USAGE_TRANSFER_DST_BIT
    , sharingMode = chooseSharingMode queueFamilyIndices
    , queueFamilyIndices = queueFamilyIndices -- ignore when sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_TO_GPU
    } acquireT
  memCopy allocator texUniformBufferAllocation (VS.fromList [texUniform, texUniform, texUniform, texUniform]) -- early free
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
  (textureStagingBuffer, textureStagingBufferAllocation, _) <- Lib.withBuffer allocator zero
    { size = fromIntegral $ (sizeOf . VS.head $ imageData pixels) * VS.length (imageData pixels)
    , usage = BUFFER_USAGE_TRANSFER_SRC_BIT
    , sharingMode = SHARING_MODE_EXCLUSIVE
    } zero
    { Vma.usage = Vma.MEMORY_USAGE_CPU_ONLY
    } acquireT
  memCopy allocator textureStagingBufferAllocation (imageData pixels)
  let textureFormat = FORMAT_R8G8B8A8_SRGB
  (textureImage, _textureImageAllocation, _) <- Lib.withImage allocator zero
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
    } acquireT

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

  textureImageView <- Lib.withImageView device textureFormat textureImage acquireT
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
  pure Present
    { vertexBuffers = [ texCoordsBuffer ]
    , indexBuffer = texIndexBuffer
    , descriptorSets = descriptorSets (textureDescriptorSetResource :: DescriptorSetResource)
    , drawSize = fromIntegral . VS.length $ texIndices
    , pipelineResource = pipelineRes
    }

withShaderStages :: MonadIO m => Device -> m (ShaderResource, V.Vector ShaderModule)
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
  Lib.createShaderResource device [ vertCode, fragCode ]

withTextureShaderStages :: MonadIO m => Device -> m (ShaderResource, V.Vector ShaderModule)
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
  Lib.createShaderResource device [ vertCode, fragCode ]

