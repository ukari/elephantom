module Elephantom.Renderer.SDL
  ( withSDL
  ) where

import qualified SDL
import qualified SDL.Video.Vulkan as SDL

import Control.Monad.Trans.Resource (MonadResource, allocate_)

withSDL :: MonadResource m => m ()
withSDL = do
  _sdlInitKey <- allocate_ (SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])) SDL.quit
  _sdlVkKey <- allocate_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
  pure ()
