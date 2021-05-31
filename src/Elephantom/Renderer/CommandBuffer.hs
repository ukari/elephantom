{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Elephantom.Renderer.CommandBuffer
  ( CommandBufferResource (..)
  , createCommandBufferResource
  , freeCommandBufferResource
  ) where

import Vulkan
import Vulkan.Zero

import qualified Data.Vector as V
import Data.Word (Word32)

import Control.Monad.IO.Class (MonadIO, liftIO)

data CommandBufferResource = CommandBufferResource
  { commandPool :: !CommandPool
  , commandBuffers :: !(V.Vector CommandBuffer)
  }

createCommandBufferResource :: MonadIO m => Device -> CommandPool -> Word32 -> m CommandBufferResource
createCommandBufferResource device commandPool frameSize = do
  commandBuffers <- allocateCommandBuffers device zero
    { commandPool = commandPool
    , level = COMMAND_BUFFER_LEVEL_PRIMARY
    , commandBufferCount = frameSize
    }
  liftIO $ print $ V.map commandBufferHandle commandBuffers
  pure CommandBufferResource
    { commandPool = commandPool
    , commandBuffers = commandBuffers
    }

freeCommandBufferResource :: MonadIO m => Device -> CommandBufferResource -> m ()
freeCommandBufferResource device CommandBufferResource {..} = freeCommandBuffers device commandPool commandBuffers
