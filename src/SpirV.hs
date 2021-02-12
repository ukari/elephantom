{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SpirV
  ( reflection
  , makeShaderInfo
  , makeDescriptorInfo
  , makeInputInfo
  ) where

import Vulkan.Zero (zero)
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Shader (ShaderModuleCreateInfo (..), ShaderModule (..))
import Vulkan.Core10.Enums.Format (Format (..))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType (..))
import Vulkan.Core10.Enums.VertexInputRate (VertexInputRate (..))
import Vulkan.Core10.DescriptorSet (DescriptorSetLayoutBinding (..), DescriptorSetLayoutCreateInfo (..))
import Vulkan.Core10.Pipeline (ShaderStageFlagBits (..), PipelineShaderStageCreateInfo (..), PipelineVertexInputStateCreateInfo (..),  VertexInputBindingDescription (..), VertexInputAttributeDescription (..))

import Foreign.Storable (Storable (sizeOf, alignment))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Aeson --(decode, eitherDecode)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess)
import System.IO.Temp (withSystemTempDirectory)
import Text.InterpolatedString.QM (qnb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString (..))
import Data.List ((\\), group, sort, sortOn, groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word32)
import Control.Applicative (liftA2)
import Control.Monad (join, ap)
import Debug.Trace (traceShow)

data EntryPoint = EntryPoint
  { name :: String
  , mode :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
  { type' :: String
  , name :: String
  , location :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic)

instance FromJSON Input where
  parseJSON = withObject "inputs" $ \v -> Input
    <$> v .: "type"
    <*> v .: "name"
    <*> v .: "location"
    <*> v .:? "array"

instance ToJSON Input where
  toJSON (Input type' name location array) = object
    [ "type" .= type'
    , "name" .= name
    , "location" .= location
    , "array" .= array
    ]
  toEncoding (Input type' name location array) = pairs
    (  "type" .= type'
    <> "name" .= name
    <> "location" .= location
    <> "array" .= array
    )

data Ubo = Ubo
  { name :: String
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic, FromJSON, ToJSON)

data Texture = Texture
  { type' :: String
  , name :: String
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic)

instance FromJSON Texture where
  parseJSON = withObject "textures" $ \v -> Texture
    <$> v .: "type"
    <*> v .: "name"
    <*> v .: "set"
    <*> v .: "binding"
    <*> v .:? "array"

instance ToJSON Texture where
  toJSON (Texture type' name set binding array) = object
    [ "type" .= type'
    , "name" .= name
    , "set" .= set
    , "binding" .= binding
    , "array" .= array
    ]
  toEncoding (Texture type' name set binding array) = pairs
    (  "type" .= type'
    <> "name" .= name
    <> "set" .= set
    <> "binding" .= binding
    <> "array" .= array
    )

data Reflection = Reflection
  { entryPoints :: Vector EntryPoint
  , inputs :: Maybe (Vector Input)
  , textures :: Maybe (Vector Texture)
  , ubos :: Maybe (Vector Ubo)
  } deriving (Show, Generic, FromJSON, ToJSON)

test :: MonadIO m => m (Maybe Reflection)
test = decode . snd <$> reflect "vert" testVert

test1 :: MonadIO m => m (Maybe Reflection)
test1 = decode . snd <$> reflect "frag" testFrag

test2 :: MonadIO m => m ()
test2 =  do
  vert <- reflection (fromString "vert") testVert
  frag <- reflection (fromString "frag") testFrag
  liftIO . print . shaderModuleCreateInfo . makeShaderInfo $ vert
  liftIO . print . shaderModuleCreateInfo . makeShaderInfo $ frag

test3 :: MonadIO m => m ()
test3 = do
  vert <- reflection (fromString "vert") testVert
  frag <- reflection (fromString "frag") testFrag
  liftIO . print . makeDescriptorInfo $ V.fromList [vert, frag]

test4 :: MonadIO m => m ()
test4 = do
  vert <- reflection (fromString "vert") testVert
  frag <- reflection (fromString "frag") testFrag
  liftIO . print . makeInputInfo $ V.fromList [vert, frag]

-- glslangValidator
-- -S <stage>  uses specified stage rather than parsing the file extension
--              choices for <stage> are vert, tesc, tese, geom, frag, or comp
data ShaderStage
  = Vert
  | Frag
  | Comp
  | Tesc
  | Tese
  | Geom
  deriving (Eq)

instance IsString ShaderStage where
  fromString = \case
    "vert" -> Vert
    "frag" -> Frag
    "comp" -> Comp
    "tesc" -> Tesc
    "tese" -> Tese
    "geom" -> Geom
    unsupport -> error $ "ShaderStage not support '" <> unsupport <> "'"

instance Show ShaderStage where
  show = \case
    Vert -> "vert"
    Frag -> "frag"
    Comp -> "comp"
    Tesc -> "tesc"
    Tese -> "tese"
    Geom -> "geom"

convertStage :: ShaderStage -> ShaderStageFlagBits
convertStage = \case
  Vert -> SHADER_STAGE_VERTEX_BIT
  Frag -> SHADER_STAGE_FRAGMENT_BIT
  Comp -> SHADER_STAGE_COMPUTE_BIT
  Tesc -> SHADER_STAGE_TESSELLATION_CONTROL_BIT
  Tese -> SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  Geom -> SHADER_STAGE_GEOMETRY_BIT

data Shader = Shader
  { stage :: ShaderStage
  , code :: B.ByteString
  } deriving (Show)

data ShaderInfo = ShaderInfo
  { shaderModuleCreateInfo:: ShaderModuleCreateInfo '[]
  , pipelineShaderStageCreateInfos :: ShaderModule -> Vector (PipelineShaderStageCreateInfo '[])
  }

makeShaderInfo :: (Shader, Reflection) -> ShaderInfo
makeShaderInfo (Shader {..}, Reflection {..}) = do
  let shaderModuleCreateInfo = makeShaderModuleCreateInfo code
  let pipelineShaderStageCreateInfos = makePipelineShaderStageCreateInfos stage entryPoints
  ShaderInfo {..}

makeDescriptorInfo :: Vector (Shader, Reflection) -> Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorInfo = makeDescriptorSetLayoutCreateInfos . join . V.map (makeDescriptorSetLayoutBindings . (stage :: Shader -> ShaderStage) . fst <*> fromMaybe [] . ubos . snd <*> fromMaybe [] . textures . snd)

makeInputInfo :: Vector (Shader, Reflection) -> Maybe (PipelineVertexInputStateCreateInfo '[])
makeInputInfo = makePipelineVertexInputStateCreateInfo . join . V.fromList . catMaybes . (inputs . snd <$>) . filter ((== Vert) . (stage :: Shader -> ShaderStage) . fst) . V.toList

makeShaderModuleCreateInfo :: "code" ::: B.ByteString -> ShaderModuleCreateInfo '[]
makeShaderModuleCreateInfo code = zero { code = code }

makePipelineShaderStageCreateInfo :: ShaderStage -> ShaderModule -> EntryPoint -> PipelineShaderStageCreateInfo '[]
makePipelineShaderStageCreateInfo stage shaderModule EntryPoint {..} = zero
  { stage = convertStage stage
  , module' = shaderModule
  , name = B.pack name
  }

makePipelineShaderStageCreateInfos :: ShaderStage -> Vector EntryPoint -> ShaderModule -> Vector (PipelineShaderStageCreateInfo '[])
makePipelineShaderStageCreateInfos stage entryPoints shaderModule = V.map (makePipelineShaderStageCreateInfo stage shaderModule) entryPoints

-- https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_vulkan_glsl.txt
-- https://vulkan.lunarg.com/doc/view/1.2.135.0/linux/tutorial/html/08-init_pipeline_layout.html
makeUboDescriptorSetLayoutBinding :: ShaderStage -> Ubo -> (Int, DescriptorSetLayoutBinding)
makeUboDescriptorSetLayoutBinding stage Ubo {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , descriptorCount = maybe 0 (V.sum . (fromIntegral <$>)) array
  , stageFlags = convertStage stage
  })

data TextureDescriptorType
  = Sampler2D

instance IsString TextureDescriptorType where
  fromString = \case
    "sampler2D" -> Sampler2D
    unsupport -> error $ "TextureDescriptorType not support '" <> unsupport <> "'"

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Sampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

makeTextureDescriptorSetLayoutBinding :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = convertTextureDescriptorType . fromString $ type'
  , descriptorCount = maybe 0 (V.sum . (fromIntegral <$>)) array
  , stageFlags = convertStage stage
  })

makeDescriptorSetLayoutBindings :: ShaderStage -> Vector Ubo -> Vector Texture -> Vector (Int, DescriptorSetLayoutBinding)
makeDescriptorSetLayoutBindings stage ubos textures = do
  let uboBindings = V.map (makeUboDescriptorSetLayoutBinding stage) ubos
  let textureBindings = V.map (makeTextureDescriptorSetLayoutBinding stage) textures
  uboBindings <> textureBindings

makeDescriptorSetLayoutCreateInfos :: Vector (Int, DescriptorSetLayoutBinding) -> V.Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorSetLayoutCreateInfos bindings = do
  let setSize = V.maximum . (fst <$>) $ bindings :: Int
  let sets :: Map Int (Vector DescriptorSetLayoutBinding)
      sets = M.fromList . map (, []) $ [ 0 .. setSize ]
  let setsMap :: Map Int (Vector DescriptorSetLayoutBinding)
      setsMap = M.fromList . map (liftA2 (,) (fst . head) (V.fromList . (snd <$>))) . groupBy ((==) `on` fst) . sortOn fst . V.toList $ bindings
  V.map makeDescriptorSetLayoutCreateInfo . V.fromList . M.elems . M.unionWith (V.++) sets $ setsMap

makeDescriptorSetLayoutCreateInfo :: Vector DescriptorSetLayoutBinding -> DescriptorSetLayoutCreateInfo '[]
makeDescriptorSetLayoutCreateInfo bindings = zero { bindings = bindings }

data VertexAttributeType
  = Vec2
  | Vec3
  | Vec4

instance IsString VertexAttributeType where
  fromString = \case
    "vec2" -> Vec2
    "vec3" -> Vec3
    "vec4" -> Vec4
    unsupport -> error $ "VertexAttributeType not support '" <> unsupport <> "'"

instance Show VertexAttributeType where
  show = \case
    Vec2 -> "vec2"
    Vec3 -> "vec3"
    Vec4 -> "vec4"

convertVertexAttributeType :: VertexAttributeType -> (Word32, Format)
convertVertexAttributeType = \case
  Vec2 -> (fromIntegral $ 2 * sizeOf (undefined :: Word32), FORMAT_R32G32_SFLOAT)
  Vec3 -> (fromIntegral $ 3 * sizeOf (undefined :: Word32), FORMAT_R32G32B32_SFLOAT)
  Vec4 -> (fromIntegral $ 4 * sizeOf (undefined :: Word32), FORMAT_R32G32B32A32_SFLOAT)

data VertexAttribute = VertexAttribute
  { binding :: Word32
  , location :: Word32
  , size :: Word32
  , format :: Format
  } deriving (Show)

-- only support single binding for input vertex
-- [SaschaWillems - Multiple Vulkan buffer binding points](https://gist.github.com/SaschaWillems/428d15ed4b5d71ead462bc63adffa93a)
-- maybe can provide a binding map parameter to specify individual binding by name, like `foo [(1, ["inPos", "inColor"]) (2, ["texCoord"])]` speficies that `(binding = 1) inPos, (binding = 1) inColor, (binding = 2) texCoord`
-- [island pipeline](https://github.com/tgfrerer/island/blob/76d0d38cba74181fa3774cef38aba4d96b6861dc/modules/le_backend_vk/le_pipeline.cpp#L21)
makePipelineVertexInputStateCreateInfo :: Vector Input -> Maybe (PipelineVertexInputStateCreateInfo '[])
makePipelineVertexInputStateCreateInfo [] = Nothing
makePipelineVertexInputStateCreateInfo inputs = do
  let vertexAttributes = join . V.map makeVertexAttribute $ inputs :: Vector VertexAttribute
  let vertexAttributeDescriptions = makeVertexInputAttributeDescriptions vertexAttributes :: Vector VertexInputAttributeDescription
  let vertexBindingDescriptions = makeVertexInputBindingDescriptions vertexAttributes :: Vector VertexInputBindingDescription
  Just zero
    { vertexBindingDescriptions
    , vertexAttributeDescriptions
    }

makeVertexInputBindingDescriptions :: Vector VertexAttribute -> Vector VertexInputBindingDescription
makeVertexInputBindingDescriptions = V.fromList . map makeVertexInputBindingDescription . calculate . groupBy ((==) `on` fst) . sortOn fst . extract
  where
    extract :: Vector VertexAttribute -> [ ("binding" ::: Int, "size" ::: Int) ]
    extract = map ((,) . fromIntegral . (binding :: VertexAttribute -> Word32) <*> fromIntegral . size) . V.toList
    calculate :: [ [ ("binding" ::: Int, "size" ::: Int) ] ] -> [ ("binding" ::: Int, "stride" ::: Int) ]
    calculate = map (liftA2 (,) (fst . head) (sum . (snd <$>)))

makeVertexInputBindingDescription :: ("binding" ::: Int, "stride" ::: Int) -> VertexInputBindingDescription
makeVertexInputBindingDescription (binding, stride) = zero
  { binding = fromIntegral binding
  , stride = fromIntegral stride
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

makeVertexAttribute :: Input -> Vector VertexAttribute
makeVertexAttribute Input {..} = do
  let count = maybe 1 V.sum array :: Int
  let (size, format) = convertVertexAttributeType . fromString $ type'
  V.map (\i -> VertexAttribute
    { binding = 0
    , location = fromIntegral . (+ location) $ i
    , size
    , format })
    [ 0 .. count - 1
    ]

makeVertexInputAttributeDescriptions :: Vector VertexAttribute -> Vector VertexInputAttributeDescription
makeVertexInputAttributeDescriptions = V.fromList . join . map process . groupBy ((==) `on` (binding :: VertexAttribute -> Word32)) . V.toList
  where
    process :: [ VertexAttribute ] -> [ VertexInputAttributeDescription ]
    process = snd . foldl (\(nextOffset, acc) cur -> liftA2 (,) fst ((acc ++) . pure . snd) (makeVertexInputAttributeDescription nextOffset cur)) (0, [])

makeVertexInputAttributeDescription :: Word32 -> VertexAttribute -> (Word32, VertexInputAttributeDescription)
makeVertexInputAttributeDescription offset VertexAttribute {..} = (offset + size, zero
  { binding
  , location
  , offset
  , format
  })

reflect :: MonadIO m => ShaderStage -> "code" ::: String -> m (B.ByteString, BL.ByteString)
reflect stage code = liftIO . withSystemTempDirectory "th-spirv" $ \dir -> do
  let shader = dir </> "test.vert"
  let spv = dir </> "vert.spv"
  writeFile shader code
  writeFile spv code
  (exitCode, _spv, err) <- readProcess . proc "glslangValidator" $ [ "-S", show stage, "-V", shader, "-o", spv]
  spirv <- B.readFile spv
  (exitCode, reflectionRaw, err) <- readProcess . proc "spirv-cross" $ [spv, "--vulkan-semantics", "--reflect"]
  pure (spirv, reflectionRaw)

reflection :: MonadIO m => ShaderStage -> "code" ::: String -> m (Shader, Reflection)
reflection stage code = do
  (spirv, reflectionRaw) <- reflect stage code
  case decode reflectionRaw of
    Just reflection -> pure (Shader {stage, code = spirv}, reflection)
    Nothing -> error "fail to reflect"

testVert :: String
testVert = [qnb|
  #version 450
  #extension GL_ARB_separate_shader_objects : enable
  const int a = 1;
  const bool b = true;
  const int[] c = {3, 4, 5, 6};
  const int d = b ? c[a]: 7;
  layout(set = 0, binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
  } ubo[d];

  layout(binding = 1) uniform sampler2D texSampler;

  layout(location = 0) in vec2 position;
  layout(location = 1) in vec4 color;
  layout(location = 2) in vec2 texCoord;

  layout(location = 0) out vec4 fragColor;
  layout(location = 1) out vec2 fragTexCoord;

  void main() {
    gl_Position = ubo[3].proj * ubo[3].view * ubo[3].model * vec4(position, 0.0, 1.0);
    fragColor = color;
    fragTexCoord = texCoord;
  }
  |]

testFrag :: String
testFrag = [qnb|
  #version 450

  #extension GL_ARB_separate_shader_objects : enable

  layout(binding = 1) uniform sampler2D texSampler[2];
  layout(set = 3, binding = 1) uniform sampler2D texSampler2;

  layout(location = 0) in vec4 fragColor;
  layout(location = 1) in vec2 fragTexCoord;

  layout(location = 0) out vec4 outColor;

  void main() {
    outColor = texture(texSampler[0], fragTexCoord); // for image use texSampler, for shape created by rasterfic use fragColor
  }
  |]
