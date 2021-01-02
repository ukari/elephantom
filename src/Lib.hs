
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
import Vulkan.Utils.ShaderQQ
import qualified VulkanMemoryAllocator as Vma

import Data.Text (Text (..))
import qualified Data.ByteString as BS
import Data.Traversable (traverse)

import Streamly
import Streamly.Prelude (drain, repeatM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, allocate_, release, register)

someFunc :: IO ()
someFunc = runResourceT $ do
  -- putStrLn "someFunc"
  --SDL.initialize [SDL.InitVideo]
  --bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit allocate
  withSDL
  window <- withWindow "test" 500 500
  extensionsCString <- SDL.vkGetInstanceExtensions window
  extensions <- liftIO $ traverse BS.packCString extensionsCString
  liftIO $ print extensions
  liftIO $ drain $ asyncly $ constRate 60 $ repeatM $ liftIO $ pure ()
  return undefined

type Managed a = forall m . MonadIO m => ResourceT m a

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

--withSurface :: SDL.Window -> Managed SurfaceKHR
--withSurface window = do
