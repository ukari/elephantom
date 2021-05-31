{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Elephantom.Renderer.Descriptor
  ( DescriptorSetResource (..)
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorSetResource
  , destroyDescriptorSetResource
  ) where

import Vulkan hiding (createDescriptorSetLayout, destroyDescriptorSetLayout)
import qualified Vulkan
import Vulkan.Zero

import Data.Word (Word32)
import qualified Data.Vector as V
import Data.List (sortOn, groupBy)
import Data.Function (on)

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO)

data DescriptorSetResource = DescriptorSetResource
  { descriptorPool :: !DescriptorPool
  , descriptorSets :: !(V.Vector DescriptorSet)
  } deriving (Show)

createDescriptorSetLayout :: MonadIO m => Device -> DescriptorSetLayoutCreateInfo '[] -> m DescriptorSetLayout
createDescriptorSetLayout = flip flip Nothing . Vulkan.createDescriptorSetLayout

destroyDescriptorSetLayout :: MonadIO m => Device -> DescriptorSetLayout -> m ()
destroyDescriptorSetLayout = flip flip Nothing . Vulkan.destroyDescriptorSetLayout

createDescriptorSetResource :: MonadIO m => Device -> V.Vector DescriptorSetLayout -> V.Vector (DescriptorSetLayoutCreateInfo '[]) -> m DescriptorSetResource
createDescriptorSetResource device descriptorSetLayouts descriptorSetLayoutCreateInfos = do
  -- https://www.reddit.com/r/vulkan/comments/8u9zqr/having_trouble_understanding_descriptor_pool/e1e8d5f?utm_source=share&utm_medium=web2x&context=3
  -- https://www.reddit.com/r/vulkan/comments/clffjm/descriptorpool_maxsets_how_does_this_work_if_you/
  -- https://www.reddit.com/r/vulkan/comments/aij7zp/there_is_a_good_technique_to_update_a_vertex/
  let descriptorPoolCreateInfo = makeDescriptorPoolCreateInfo (fromIntegral . length $ descriptorSetLayouts) descriptorSetLayoutCreateInfos
  descriptorPool <- createDescriptorPool device descriptorPoolCreateInfo Nothing 
  descriptorSets <- allocateDescriptorSets device zero
    { descriptorPool = descriptorPool
    , setLayouts = descriptorSetLayouts
    }
  pure DescriptorSetResource {..}

destroyDescriptorSetResource :: MonadIO m => Device -> DescriptorSetResource -> m ()
destroyDescriptorSetResource device DescriptorSetResource {..} = do
  freeDescriptorSets device descriptorPool descriptorSets
  destroyDescriptorPool device descriptorPool Nothing

makeDescriptorPoolCreateInfo :: Word32 -> V.Vector (DescriptorSetLayoutCreateInfo '[]) -> DescriptorPoolCreateInfo '[]
makeDescriptorPoolCreateInfo maxSets infos = zero
  { poolSizes = V.fromList
    [ zero
      { type' = t
      , descriptorCount = fromIntegral n
      }
    | (t, n) <- analyse infos ]
  , maxSets = maxSets
  , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT -- VUID-vkFreeDescriptorSets-descriptorPool-00312: descriptorPool must have been created with the VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag
  }
  where
    analyse :: V.Vector (DescriptorSetLayoutCreateInfo '[]) -> [(DescriptorType, Int)]
    analyse = map calculate . groupBy ((==) `on` fst) . sortOn fst . extract
    extract :: V.Vector (DescriptorSetLayoutCreateInfo '[]) -> [(DescriptorType, Int)]
    extract = map (liftA2 (,) tname (fromIntegral . dcount)) . V.toList . (bindings =<<)
    calculate :: [(DescriptorType, Int)] -> (DescriptorType, Int)
    calculate = liftA2 (,) (fst . head) (sum . (snd <$>))
    tname = descriptorType :: DescriptorSetLayoutBinding -> DescriptorType
    dcount = descriptorCount :: DescriptorSetLayoutBinding -> Word32
