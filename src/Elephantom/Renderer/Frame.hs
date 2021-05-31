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
import Control.Concurrent (MVar, readMVar)
import Control.Exception (throw)
import qualified Control.Exception as Ex

import Elephantom.Renderer.CommandBuffer (CommandBufferResource (..), createCommandBufferResource, freeCommandBufferResource)
import Elephantom.Renderer.Swapchain (SwapchainResource (..), createSwapchain, destroySwapchain)
import Elephantom.Renderer.Present (Present)
import Elephantom.Renderer.Command (submitCommand)

data Context = Context
  { phys :: !PhysicalDevice
  , device :: !Device
  , graphicsQueue :: !Queue
  , presentQueue :: !Queue
  , window :: !SDL.Window
  , surf :: !SurfaceKHR
  , queueFamilyIndices :: !(V.Vector Word32)
  , surfaceFormat :: !SurfaceFormatKHR
  , renderPass :: !RenderPass
  , presentsMVar :: !(MVar (V.Vector Present))
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

recreateSwapchain :: MonadIO m => Context -> FrameSync -> CommandBufferResource -> SwapchainResource -> m (FrameSync, CommandBufferResource, SwapchainResource)
recreateSwapchain Context {..} oldFrameSync@FrameSync {..} oldCmdRes@CommandBufferResource {..} oldSwapchainRes@SwapchainResource { swapchain } = do
  --deviceWaitIdleSafe device
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  liftIO . print $ "recreate width " <> show extent
  -- let fence = submitFinishedFences ! sync
  -- --Vulkan.resetFences device [ fence ]
  -- --queueSubmit graphicsQueue [] fence
  -- liftIO . print $ "reset fence " <> show fence
  swapchainRes@SwapchainResource { framebuffers } <- createSwapchain phys device surf surfaceFormat queueFamilyIndices extent renderPass swapchain
  destroySwapchain device oldSwapchainRes
  frameSync <- createFrameSync device framebuffers
  destroyFrameSync device oldFrameSync
  freeCommandBufferResource device oldCmdRes
  commandBufferRes <- createCommandBufferResource device commandPool frameSize
  deviceWaitIdleSafe device
  pure (frameSync, commandBufferRes, swapchainRes)

drawFrameHandler :: (MonadIO m) => Context -> FrameSync -> CommandBufferResource -> SwapchainResource -> VulkanException -> m (Maybe (Context, FrameSync, CommandBufferResource, SwapchainResource))
drawFrameHandler ctx frame cmdr swpr (VulkanException _e@ERROR_OUT_OF_DATE_KHR) = do
  (frameSync, commandBufferRes, swapchainRes) <- recreateSwapchain ctx frame cmdr swpr
  pure . Just $ (ctx, frameSync, commandBufferRes, swapchainRes)
drawFrameHandler _ _ _ _ e = do
  liftIO . print $ "throw " <> show e
  throw e

drawFrame :: (MonadIO m) => (Context, FrameSync, CommandBufferResource, SwapchainResource) -> m (Maybe (Context, FrameSync, CommandBufferResource, SwapchainResource))
drawFrame (ctx@Context {..}, frameSync@FrameSync {..}, cmdr@CommandBufferResource {..}, swpr@SwapchainResource {..}) = (fmap liftIO . Ex.handle) (drawFrameHandler ctx frameSync cmdr swpr) $ do
  
  V2 width height <- SDL.vkGetDrawableSize window
  let extent = Extent2D (fromIntegral width) (fromIntegral height)
  liftIO . print $ "width " <> show extent
  -- V2 w h <- StateVar.get $ SDL.windowSize window
  -- liftIO . print $ "window " <> show w <> " " <> show h
  liftIO . print $ "finish report"
  
  let commandBuffer = commandBuffers ! sync
  presents <- liftIO . readMVar $ presentsMVar
  -- deviceWaitIdleSafe device
  -- resetCommandBuffer commandBuffer COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  submitCommand extent renderPass presents (commandBuffer, framebuffers! sync)
  let imageAvailableSemaphore = imageAvailableSemaphores ! sync
  let renderFinishedSemaphore = renderFinishedSemaphores ! sync
  let fence = submitFinishedFences ! sync
  liftIO . print $ "wait for fence " <> show fence
  _ <- waitForFencesSafe device [ fence ] True maxBound
  liftIO . print $ "before reset"
  resetFences device [ fence ]
  liftIO . print $ "before acquire"
  imageIndex <- snd <$> acquireNextImageKHRSafe device swapchain maxBound imageAvailableSemaphore zero
  liftIO . print $ "submit"
  queueSubmit graphicsQueue
    [ SomeStruct $ zero
      { Core10.waitSemaphores = [ imageAvailableSemaphore ]
      , waitDstStageMask = [ PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT ]
      , commandBuffers = [ commandBufferHandle commandBuffer ]
      , signalSemaphores = [ renderFinishedSemaphore ]
      }
    ] fence
  liftIO . print $ "present"
  _ <- queuePresentKHR presentQueue zero
    { Swap.waitSemaphores = [ renderFinishedSemaphore ]
    , swapchains = [ swapchain ]
    , imageIndices = [ imageIndex ]
    }
  liftIO . print $ "end frame"
  -- queueWaitIdle presentQueue
  -- queueWaitIdle graphicsQueue
  pure . Just $
    ( ctx
    , frameSync
      { sync = (sync + 1) `mod` fromIntegral frameSize
      }
    , cmdr
    , swpr
    )
