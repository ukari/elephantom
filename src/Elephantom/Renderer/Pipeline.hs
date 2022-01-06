{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}

module Elephantom.Renderer.Pipeline
  ( PipelineResource (..)
  , defaultPipelineCreateInfo
  , createPipelineResource
  , destroyPipelineResource
  , withPipelineResource
  ) where

import Vulkan
import Vulkan.Zero
import Vulkan.CStruct.Extends

import Data.Bits ((.|.))
import Data.Vector ((!))

import Control.Monad.IO.Class (MonadIO, liftIO)

import Elephantom.Renderer.Allocator (Allocator)
import Elephantom.Renderer.Shader (ShaderResource (..))

data PipelineResource = PipelineResource
  { pipeline :: !Pipeline
  , pipelineLayout :: !PipelineLayout
  } deriving (Show)

-- | defaultPipelineCreateInfo
-- stages
-- vertexInputState
-- layout
-- renderPass
defaultPipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
defaultPipelineCreateInfo = zero
  { stages = []
  , vertexInputState = Nothing
  , inputAssemblyState = Just zero
    { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
    , primitiveRestartEnable = False
    }
  , viewportState = Just $ SomeStruct zero
    { viewportCount = 1
    , scissorCount = 1
    }
  , rasterizationState = SomeStruct $ zero
    { depthClampEnable = False
    , rasterizerDiscardEnable = False
    , lineWidth = 1
    , polygonMode = POLYGON_MODE_FILL
    , cullMode = CULL_MODE_NONE -- NOTE: for 2D pipeline, no cull mode. while for 3D pipeline, needs set CULL_MODE_BACK_BIT
    , frontFace = FRONT_FACE_CLOCKWISE
    , depthBiasEnable = False
    }
  , multisampleState = Just $ SomeStruct $ zero
    { sampleShadingEnable = False
    , rasterizationSamples = SAMPLE_COUNT_1_BIT
    , minSampleShading = 1
    , sampleMask = [ maxBound ]
    }
  , depthStencilState = Nothing
  , colorBlendState = Just $ SomeStruct $ zero
    { logicOpEnable = False
    , attachments =
      [ zero
        { colorWriteMask = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT
        , blendEnable = True
        , srcColorBlendFactor = BLEND_FACTOR_SRC_ALPHA
        , dstColorBlendFactor = BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
        , colorBlendOp = BLEND_OP_ADD
        , srcAlphaBlendFactor = BLEND_FACTOR_ONE
        , dstAlphaBlendFactor = BLEND_FACTOR_ZERO
        , alphaBlendOp = BLEND_OP_ADD
        } ] }
  , dynamicState = Just $ zero
    { dynamicStates =
      [ DYNAMIC_STATE_VIEWPORT
      , DYNAMIC_STATE_SCISSOR ] }
  , layout = zero
  , renderPass = zero
  , subpass = 0
  , basePipelineHandle = zero
  }

createPipelineResource :: MonadIO m => Device -> RenderPass -> ShaderResource -> GraphicsPipelineCreateInfo '[] -> m PipelineResource
createPipelineResource device renderPass ShaderResource {..} pipelineCreateInfo = do
  -- https://stackoverflow.com/questions/56928041/what-is-the-purpose-of-multiple-setlayoutcounts-of-vulkan-vkpipelinelayoutcreate
  -- https://vulkan.lunarg.com/doc/view/1.2.135.0/linux/tutorial/html/08-init_pipeline_layout.html
  pipelineLayout <- Vulkan.createPipelineLayout device zero
    { setLayouts = descriptorSetLayouts
    } Nothing
  liftIO $ print pipelineLayout
  (_result, pipelines) <- Vulkan.createGraphicsPipelines device zero
    [ SomeStruct $ pipelineCreateInfo
      { stages = shaderStages
      , vertexInputState = vertexInputState
      , layout = pipelineLayout
      , renderPass = renderPass
      }
    ] Nothing
  let pipeline = pipelines ! 0
  pure PipelineResource {..}

destroyPipelineResource :: MonadIO m => Device -> PipelineResource -> m ()
destroyPipelineResource device PipelineResource {..} = do
  Vulkan.destroyPipeline device pipeline Nothing
  Vulkan.destroyPipelineLayout device pipelineLayout Nothing

withPipelineResource :: MonadIO m => Device -> RenderPass -> ShaderResource -> GraphicsPipelineCreateInfo '[] -> Allocator m PipelineResource
withPipelineResource device renderPass shaderResource pipelineCreateInfo allocate = allocate (createPipelineResource device renderPass shaderResource pipelineCreateInfo) (destroyPipelineResource device)
