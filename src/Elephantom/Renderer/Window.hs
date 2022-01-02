{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.Window
  ( createWindow
  , destroyWindow
  , withWindow
  ) where

import qualified SDL

import Data.Text (Text)

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Allocator (Allocator)

createWindow :: MonadIO m => Text -> Int -> Int -> m SDL.Window
createWindow title width height = do
  window <- SDL.createWindow title SDL.defaultWindow
    { SDL.windowInitialSize = SDL.V2 (fromIntegral width) (fromIntegral height)
    , SDL.windowGraphicsContext = SDL.VulkanContext
    , SDL.windowResizable = True--False
    , SDL.windowMode = SDL.Windowed
    , SDL.windowPosition = SDL.Centered
    , SDL.windowBorder = False--True
    , SDL.windowHighDPI = True
    }
  SDL.windowMinimumSize window SDL.$= SDL.V2 (fromIntegral width) (fromIntegral height)
  pure window

{-# INLINE destroyWindow #-}
destroyWindow :: MonadIO m => SDL.Window -> m ()
destroyWindow = SDL.destroyWindow

withWindow :: MonadIO m => Text -> Int -> Int -> Allocator m SDL.Window
withWindow title width height allocate = allocate (createWindow title width height) destroyWindow
