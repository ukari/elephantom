module Elephantom.Renderer.Surface
  ( createSurface
  , destroySurface
  , withSurface
  ) where

import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan (Instance, SurfaceKHR (..), instanceHandle, destroySurfaceKHR)

import Foreign.Ptr ( castPtr)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)

createSurface :: MonadIO m => Instance -> SDL.Window -> m SurfaceKHR
createSurface inst window = SurfaceKHR <$> SDL.vkCreateSurface window (castPtr . instanceHandle $ inst)

destroySurface :: MonadIO m => Instance -> SurfaceKHR -> m ()
destroySurface = flip flip Nothing . destroySurfaceKHR

withSurface :: MonadResource m => Instance -> SDL.Window -> m SurfaceKHR
withSurface inst window = snd <$> allocate (createSurface inst window) (destroySurface inst)
