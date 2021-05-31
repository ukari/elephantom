{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Elephantom.Renderer.CommandPool
  ( CommandPoolResource (..)
  , createCommandPoolResource
  , destroyCommandPoolResource
  , withCommandPoolResource
  ) where

import Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)

import Elephantom.Renderer.QueueFamily (QueueFamilyIndices (..))

data CommandPoolResource = CommandPoolResource
  { graphicsCommandPool :: CommandPool
  , transferCommandPool :: CommandPool
  } deriving (Show)

createCommandPoolResource :: MonadIO m => Device -> QueueFamilyIndices -> m CommandPoolResource
createCommandPoolResource device QueueFamilyIndices {..} = do
  graphicsCommandPool <- createCommandPool device zero
    { queueFamilyIndex = graphicsFamily
    , flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT --zeroBits
    } Nothing
  transferCommandPool <- createCommandPool device zero
    { queueFamilyIndex = transferFamily
    , flags = COMMAND_POOL_CREATE_TRANSIENT_BIT
    } Nothing
  pure CommandPoolResource {..}

destroyCommandPoolResource :: MonadIO m => Device -> CommandPoolResource -> m ()
destroyCommandPoolResource device CommandPoolResource {..} = do
  destroyCommandPool device graphicsCommandPool Nothing
  destroyCommandPool device transferCommandPool Nothing

withCommandPoolResource :: MonadResource m => Device  -> QueueFamilyIndices -> m CommandPoolResource
withCommandPoolResource device queueFamilyIndices = snd <$> allocate (createCommandPoolResource device queueFamilyIndices) (destroyCommandPoolResource device)
