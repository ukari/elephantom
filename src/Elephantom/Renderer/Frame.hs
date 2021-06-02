{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Frame
  ( Context (..)
  , FrameSync (..)
  , createFrameSync
  , destroyFrameSync
  , recreateSwapchain
  , drawFrameHandler
  , drawFrame
  , cleanupFrame
  ) where

import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan
import Vulkan.Zero
import Vulkan.CStruct.Extends
import qualified Vulkan.Core10 as Core10
import qualified Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Exception

import Linear (V2 (..))
import Data.Word (Word32)
import Data.Vector ((!))
import qualified Data.Vector as V

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, readMVar, swapMVar)
import Control.Exception (AsyncException (..), SomeAsyncException (..), throw)
import qualified Control.Exception as Ex

import Elephantom.Application (Application)
import Elephantom.Renderer.CommandBuffer (CommandBufferResource (..), createCommandBufferResource, freeCommandBufferResource)
import Elephantom.Renderer.Swapchain (SwapchainResource (..), createSwapchain, destroySwapchain)
import Elephantom.Renderer.Present (Present)
import Elephantom.Renderer.Command (submitCommand)

data Context = Context
  { application :: !Application
  , phys :: !PhysicalDevice
  , device :: !Device
  , graphicsQueue :: !Queue
  , presentQueue :: !Queue
  , window :: !SDL.Window
  , surf :: !SurfaceKHR
  , queueFamilyIndices :: !(V.Vector Word32)
  , surfaceFormat :: !SurfaceFormatKHR
  , renderPass :: !RenderPass
  , presentsMVar :: !(MVar (V.Vector Present))
  , cleanupMVar :: !(MVar (FrameSync, CommandBufferResource, SwapchainResource))
  }

data FrameSync = FrameSync
  { imageAvailableSemaphores :: V.Vector Semaphore
  , renderFinishedSemaphores :: V.Vector Semaphore
  , submitFinishedFences :: V.Vector Fence
  , frameSize :: Word32
  , sync :: Int
  }

createFrameSync :: MonadIO m => Device -> V.Vector Framebuffer -> m FrameSync
createFrameSync device framebuffers = do
  imageAvailableSemaphores <- mapM (const makeSemaphore) framebuffers
  renderFinishedSemaphores <- mapM (const makeSemaphore) framebuffers
  submitFinishedFences <- mapM (const makeFence) framebuffers
  let frameSize = fromIntegral . length $ framebuffers
  let sync = 0
  pure FrameSync {..}
  where
    makeSemaphore = createSemaphore device zero Nothing
    makeFence = createFence device zero { flags = FENCE_CREATE_SIGNALED_BIT } Nothing

destroyFrameSync :: MonadIO m => Device -> FrameSync -> m ()
destroyFrameSync device FrameSync {..} = do
  mapM_ freeSemaphore imageAvailableSemaphores
  mapM_ freeSemaphore renderFinishedSemaphores
  mapM_ freeFence submitFinishedFences
  where
    freeSemaphore = flip (destroySemaphore device) Nothing
    freeFence = flip (destroyFence device) Nothing

recreateSwapchain :: MonadIO m => (Context, FrameSync, CommandBufferResource, SwapchainResource) -> m (FrameSync, CommandBufferResource, SwapchainResource)
recreateSwapchain (Context {..}, oldFrameSync@FrameSync {..}, oldCmdRes@CommandBufferResource {..}, oldSwapchainRes@SwapchainResource { swapchain }) = do
  deviceWaitIdleSafe device

  freeCommandBufferResource device oldCmdRes
  commandBufferRes <- createCommandBufferResource device commandPool frameSize

  destroyFrameSync device oldFrameSync

  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  swapchainRes@SwapchainResource { framebuffers } <- createSwapchain phys device surf surfaceFormat queueFamilyIndices extent renderPass swapchain
  destroySwapchain device oldSwapchainRes

  frameSync <- createFrameSync device framebuffers
  _ <- liftIO . swapMVar cleanupMVar $ (frameSync, commandBufferRes, swapchainRes) 
  deviceWaitIdleSafe device
  pure (frameSync, commandBufferRes, swapchainRes)

drawFrameHandler :: MonadIO m => (Context, FrameSync, CommandBufferResource, SwapchainResource) -> VulkanException -> m (Maybe (Context, FrameSync, CommandBufferResource, SwapchainResource))
drawFrameHandler frame@(ctx, _, _, _) (VulkanException _e@ERROR_OUT_OF_DATE_KHR) = do
  (frameSync, commandBufferRes, swapchainRes) <- recreateSwapchain frame
  pure . Just $ (ctx, frameSync, commandBufferRes, swapchainRes)
drawFrameHandler _ e = throw e

drawFrame :: MonadIO m => (Context, FrameSync, CommandBufferResource, SwapchainResource) -> m (Maybe (Context, FrameSync, CommandBufferResource, SwapchainResource))
drawFrame frame@(ctx@Context {..}, frameSync@FrameSync {..}, cmdr@CommandBufferResource {..}, swpr@SwapchainResource {..}) = (fmap liftIO . Ex.handle) (drawFrameHandler frame) $ do
  let imageAvailableSemaphore = imageAvailableSemaphores ! sync
  imageIndex <- snd <$> acquireNextImageKHRSafe device swapchain maxBound imageAvailableSemaphore zero

  presents <- liftIO . readMVar $ presentsMVar

  let fence = submitFinishedFences ! sync
  _ <- waitForFencesSafe device [ fence ] True maxBound
  resetFences device [ fence ]

  let commandBuffer = commandBuffers ! sync
  resetCommandBuffer commandBuffer COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  submitCommand application extent renderPass presents (commandBuffer, framebuffers! sync)

  let renderFinishedSemaphore = renderFinishedSemaphores ! sync
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

  pure . Just $
    ( ctx
    , frameSync
      { sync = (sync + 1) `mod` fromIntegral frameSize
      }
    , cmdr
    , swpr
    )

cleanupFrame :: MonadIO m => Device -> MVar (FrameSync, CommandBufferResource, SwapchainResource) -> m ()
cleanupFrame device cleanupMVar = do
  (frameSync, cmdr, swpr) <- liftIO . readMVar $ cleanupMVar
  deviceWaitIdleSafe device
  freeCommandBufferResource device cmdr
  destroyFrameSync device frameSync
  destroySwapchain device swpr
