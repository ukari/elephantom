{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Elephantom.Renderer.Shader
  ( ShaderResource (..)
  , createShaderModule
  , destroyShaderModule
  , createShaderResource
  , destroyShaderResource
  ) where

import Vulkan hiding (createShaderModule, destroyShaderModule, createDescriptorSetLayout, destroyDescriptorSetLayout)
import qualified Vulkan
import Vulkan.CStruct.Extends

import Data.Bifunctor (first)
import qualified Data.Vector as V
import Data.ByteString.Char8 (ByteString)

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)

import Elephantom.Renderer.Descriptor (createDescriptorSetLayout, destroyDescriptorSetLayout)
import SpirV

data ShaderResource = ShaderResource
  { shaderStages :: !(V.Vector (SomeStruct PipelineShaderStageCreateInfo))
  , descriptorSetLayouts :: !(V.Vector DescriptorSetLayout)
  , descriptorSetLayoutCreateInfos :: !(V.Vector (DescriptorSetLayoutCreateInfo '[]))
  , vertexInputState :: !(Maybe (SomeStruct PipelineVertexInputStateCreateInfo))
  , shaderModules :: !(V.Vector ShaderModule)
  }
  deriving (Show)

createShaderModule :: MonadIO m => Device -> SpirV.ShaderInfo -> m ShaderModule
createShaderModule device SpirV.ShaderInfo {..} = Vulkan.createShaderModule device shaderModuleCreateInfo Nothing

destroyShaderModule :: MonadIO m => Device -> ShaderModule -> m ()
destroyShaderModule = flip flip Nothing . Vulkan.destroyShaderModule

createShaderResource :: MonadIO m => Device -> V.Vector ("spirv" ::: ByteString) -> m (ShaderResource, V.Vector ShaderModule)
createShaderResource device spirvs = do
  -- the reflection should be done in compile stage
  reflects <- V.mapM SpirV.reflection' spirvs
  let shaderInfos = SpirV.makeShaderInfo <$> reflects
  (shaderStages, shaderModules) <- first join . V.unzip <$> mapM (liftA2 consfmap SpirV.pipelineShaderStageCreateInfos (createShaderModule device)) shaderInfos
  let descriptorSetLayoutCreateInfos = SpirV.makeDescriptorInfo reflects
  descriptorSetLayouts <- mapM (createDescriptorSetLayout device) descriptorSetLayoutCreateInfos
  let vertexInputState = SpirV.makeInputInfo reflects
  pure (ShaderResource {..}, shaderModules)

destroyShaderResource :: MonadIO m => Device -> ShaderResource -> m ()
destroyShaderResource device ShaderResource {..} = mapM_ (destroyDescriptorSetLayout device) descriptorSetLayouts

consfmap :: (Monad f, Functor f) => (a -> b) -> f a -> f (b, a)
consfmap f fa = do
  a <- fa
  pure (f a, a)
