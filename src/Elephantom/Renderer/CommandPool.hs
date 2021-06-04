{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.CommandPool
  ( CommandPoolResource (..)
  , createCommandPoolResource
  , destroyCommandPoolResource
  , withCommandPoolResource
  ) where

import Vulkan
import Vulkan.Zero

import Data.Bits ((.|.))

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Allocator (Allocator)
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
  -- https://www.khronos.org/registry/vulkan/specs/1.1-khr-extensions/html/chap6.html
  -- VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT allows any command buffer allocated from a pool to be individually reset to the initial state; either by calling vkResetCommandBuffer, or via the implicit reset when calling vkBeginCommandBuffer. If this flag is not set on a pool, then vkResetCommandBuffer must not be called for any command buffer allocated from that pool.
  transferCommandPool <- createCommandPool device zero
    { queueFamilyIndex = transferFamily
    , flags = COMMAND_POOL_CREATE_TRANSIENT_BIT .|. COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT -- 
    } Nothing
  pure CommandPoolResource {..}

destroyCommandPoolResource :: MonadIO m => Device -> CommandPoolResource -> m ()
destroyCommandPoolResource device CommandPoolResource {..} = do
  destroyCommandPool device graphicsCommandPool Nothing
  destroyCommandPool device transferCommandPool Nothing

withCommandPoolResource :: MonadIO m => Device -> QueueFamilyIndices -> Allocator m CommandPoolResource
withCommandPoolResource device queueFamilyIndices allocate = allocate (createCommandPoolResource device queueFamilyIndices) (destroyCommandPoolResource device)
