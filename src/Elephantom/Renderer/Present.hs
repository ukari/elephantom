module Elephantom.Renderer.Present
  ( Present (..)
  ) where

import Vulkan

import Data.Word (Word32)
import qualified Data.Vector as V

import Elephantom.Renderer.Pipeline (PipelineResource)

data Present = Present
  { vertexBuffers :: !(V.Vector Buffer)
  , indexBuffer :: !Buffer
  , descriptorSets :: !(V.Vector DescriptorSet)
  , drawSize :: !Word32
  , pipelineResource :: !PipelineResource
  }
