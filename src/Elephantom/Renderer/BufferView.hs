{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Elephantom.Renderer.BufferView
  ( createBufferView
  , destroyBufferView
  , withBufferView
  ) where

import Vulkan hiding (createBufferView, destroyBufferView, withBufferView)
import qualified Vulkan
import Vulkan.Zero

import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Allocator (Allocator)

createBufferView :: MonadIO m => Device -> Format -> Buffer -> m BufferView
createBufferView device format buf = Vulkan.createBufferView device zero
  { buffer = buf
  , format = format
  , offset = 0
  , range = WHOLE_SIZE
  } Nothing

destroyBufferView :: MonadIO m => Device -> BufferView -> m ()
destroyBufferView = flip flip Nothing . Vulkan.destroyBufferView

withBufferView :: MonadIO m => Device -> Format -> Buffer -> Allocator m BufferView
withBufferView device format buf allocate = allocate (createBufferView device format buf) (destroyBufferView device)
