{-# LANGUAGE RecordWildCards #-}

module Elephantom.Renderer.Queue
  ( QueueResource (..)
  , getQueueResource
  ) where

import Vulkan

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.QueueFamily (QueueFamilyIndices (..))

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
