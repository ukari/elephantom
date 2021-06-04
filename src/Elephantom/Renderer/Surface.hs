{-# LANGUAGE RankNTypes #-}

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

import Elephantom.Renderer.Allocator (Allocator)

createSurface :: MonadIO m => Instance -> SDL.Window -> m SurfaceKHR
createSurface inst window = SurfaceKHR <$> SDL.vkCreateSurface window (castPtr . instanceHandle $ inst)

destroySurface :: MonadIO m => Instance -> SurfaceKHR -> m ()
destroySurface = flip flip Nothing . destroySurfaceKHR

withSurface :: MonadIO m => Instance -> SDL.Window -> Allocator m SurfaceKHR
withSurface inst window allocate = allocate (createSurface inst window) (destroySurface inst)
