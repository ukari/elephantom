{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SpirV
  ( ShaderStage (..)
  , ShaderInfo (..)
  , reflection
  , reflection'
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
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Aeson ((.=), (.:), (.:?), FromJSON (..), ToJSON (..), Value (String), decode, decode', encode, eitherDecode, object, pairs, withObject, withText)
import Data.Aeson.Types (prependFailure, typeMismatch, unexpected, parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Text.InterpolatedString.QM (qnb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List ((\\), group, sort, sortOn, groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word32)
import Control.Applicative (liftA2)
import Control.Monad (join, ap)
import Control.Exception (Exception (..), SomeException (..), throw, catch, handle, evaluate)
import Debug.Trace (traceShow)

data EntryPoint = EntryPoint
  { name :: Text
  , mode :: ShaderStage
  } deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
  { type' :: Text
  , name :: Text
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
  { name :: Text
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic, FromJSON, ToJSON)

data Texture = Texture
  { type' :: Text
  , name :: Text
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
test = decode . snd <$> reflect (from "vert") testVert

test0 :: Maybe Reflection
test0 = Just (Reflection {entryPoints = [EntryPoint {name = "main", mode = Frag}], inputs = Just [Input {type' = "vec2", name = "fragTexCoord", location = 1, array = Nothing},Input {type' = "vec4", name = "fragColor", location = 0, array = Nothing}], textures = Just [Texture {type' = "sampler2D", name = "texSampler", set = 0, binding = 1, array = Just [2]},Texture {type' = "sampler2D", name = "texSampler2", set = 3, binding = 1, array = Nothing}], ubos = Nothing})

test0Encode :: BL.ByteString
test0Encode = "{\"textures\":[{\"set\":0,\"array\":[2],\"name\":\"texSampler\",\"type\":\"sampler2D\",\"binding\":1},{\"set\":3,\"array\":null,\"name\":\"texSampler2\",\"type\":\"sampler2D\",\"binding\":1}],\"inputs\":[{\"location\":1,\"array\":null,\"name\":\"fragTexCoord\",\"type\":\"vec2\"},{\"location\":0,\"array\":null,\"name\":\"fragColor\",\"type\":\"vec4\"}],\"ubos\":null,\"entryPoints\":[{\"mode\":\"frag\",\"name\":\"main\"}]}"

test1 :: MonadIO m => m (Maybe Reflection)
test1 = decode . snd <$> reflect (from "frag") testFrag

test2 :: MonadIO m => m ()
test2 =  do
  vert <- reflection (from "vert") testVert
  frag <- reflection (from "frag") testFrag
  liftIO . print . shaderModuleCreateInfo . makeShaderInfo $ vert
  liftIO . print . shaderModuleCreateInfo . makeShaderInfo $ frag

test3 :: MonadIO m => m ()
test3 = do
  vert <- reflection (from "vert") testVert
  frag <- reflection (from "frag") testFrag
  liftIO . print . makeDescriptorInfo $ V.fromList [vert, frag]

test4 :: MonadIO m => m ()
test4 = do
  vert <- reflection (from "vert") testVert
  frag <- reflection (from "frag") testFrag
  liftIO . print . makeInputInfo $ V.fromList [vert, frag]

data ConvertException = ConvertException Text Text
  deriving (Show)

instance Exception ConvertException

class Convert a where
  from :: Text -> a
  to :: a -> Text

checkConvert :: forall a . Convert a => Text -> Bool
checkConvert x = unsafePerformIO $ handle (\(ConvertException _raw _err) -> pure True) (evaluate . flip seq False . (from :: Text -> a) $ x)

convertErrorMessage :: forall a . Convert a => Text -> Text
convertErrorMessage x = unsafePerformIO $ handle (\(ConvertException _raw err) -> pure err) (evaluate . flip seq "" . (from :: Text -> a) $ x)

data ShaderStage
  = Vert
  | Frag
  | Comp
  | Tesc
  | Tese
  | Geom
  | Rgen
  | Rint
  | Rahit
  | Rchit
  | Rmiss
  | Rcall
  | Task
  | Mesh
  deriving (Eq, Show)

instance Convert ShaderStage where
  from = \case
    "vert" -> Vert
    "frag" -> Frag
    "comp" -> Comp
    "tesc" -> Tesc
    "tese" -> Tese
    "geom" -> Geom
    "rgen" -> Rgen
    "rint" -> Rint
    "rahit" -> Rahit
    "rchit" -> Rchit
    "rmiss" -> Rmiss
    "rcall" -> Rcall
    "task" -> Task
    "mesh" -> Mesh
    unsupport -> throw $ ConvertException unsupport $ "ShaderStage not support '" <> unsupport <> "'"
  to = \case
    Vert -> "vert"
    Frag -> "frag"
    Comp -> "comp"
    Tesc -> "tesc"
    Tese -> "tese"
    Geom -> "geom"
    Rgen -> "rgen"
    Rint -> "rint"
    Rahit -> "rahit"
    Rchit -> "rchit"
    Rmiss -> "rmiss"
    Rcall -> "rcall"
    Task -> "task"
    Mesh -> "mesh"

instance FromJSON ShaderStage where
  parseJSON value@(String x) | checkConvert @ShaderStage x = prependFailure (T.unpack . convertErrorMessage  @ShaderStage $ x) . unexpected $ value
                             | otherwise = withText "mode" (pure . from) value
  parseJSON x = withText "mode" (pure . from) x

instance ToJSON ShaderStage where
  toJSON = String . to
  toEncoding = toEncoding . to

convertStage :: ShaderStage -> ShaderStageFlagBits
convertStage = \case
  Vert -> SHADER_STAGE_VERTEX_BIT
  Frag -> SHADER_STAGE_FRAGMENT_BIT
  Comp -> SHADER_STAGE_COMPUTE_BIT
  Tesc -> SHADER_STAGE_TESSELLATION_CONTROL_BIT
  Tese -> SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  Geom -> SHADER_STAGE_GEOMETRY_BIT
  Rgen -> SHADER_STAGE_RAYGEN_BIT_KHR
  Rint -> SHADER_STAGE_INTERSECTION_BIT_KHR
  Rahit -> SHADER_STAGE_ANY_HIT_BIT_KHR
  Rchit -> SHADER_STAGE_CLOSEST_HIT_BIT_KHR
  Rmiss -> SHADER_STAGE_MISS_BIT_KHR
  Rcall -> SHADER_STAGE_CALLABLE_BIT_KHR
  Task -> SHADER_STAGE_TASK_BIT_NV
  Mesh -> SHADER_STAGE_MESH_BIT_NV

data Shader = Shader
  { stage :: ShaderStage
  , code :: B.ByteString
  } deriving (Show)

data ShaderInfo = ShaderInfo
  { shaderModuleCreateInfo:: ShaderModuleCreateInfo '[]
  , pipelineShaderStageCreateInfos :: ShaderModule -> Vector (SomeStruct PipelineShaderStageCreateInfo)
  }

makeShaderInfo :: (Shader, Reflection) -> ShaderInfo
makeShaderInfo (Shader {..}, Reflection {..}) = ShaderInfo {..}
  where
    shaderModuleCreateInfo = makeShaderModuleCreateInfo code
    pipelineShaderStageCreateInfos = (SomeStruct <$>) . makePipelineShaderStageCreateInfos stage entryPoints

makeDescriptorInfo :: Vector (Shader, Reflection) -> Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorInfo = makeDescriptorSetLayoutCreateInfos . join . V.map (makeDescriptorSetLayoutBindings . (stage :: Shader -> ShaderStage) . fst <*> fromMaybe [] . ubos . snd <*> fromMaybe [] . textures . snd)

makeInputInfo :: Vector (Shader, Reflection) -> Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
makeInputInfo = (SomeStruct <$>) . makePipelineVertexInputStateCreateInfo . join . V.fromList . catMaybes . (inputs . snd <$>) . filter ((== Vert) . (stage :: Shader -> ShaderStage) . fst) . V.toList

makeShaderModuleCreateInfo :: "code" ::: B.ByteString -> ShaderModuleCreateInfo '[]
makeShaderModuleCreateInfo code = zero { code = code }

makePipelineShaderStageCreateInfo :: ShaderStage -> ShaderModule -> EntryPoint -> PipelineShaderStageCreateInfo '[]
makePipelineShaderStageCreateInfo stage shaderModule EntryPoint {..} = zero
  { stage = convertStage stage
  , module' = shaderModule
  , name = B.pack . T.unpack $ name
  }

makePipelineShaderStageCreateInfos :: ShaderStage -> Vector EntryPoint -> ShaderModule -> Vector (PipelineShaderStageCreateInfo '[])
makePipelineShaderStageCreateInfos stage entryPoints shaderModule = V.map (makePipelineShaderStageCreateInfo stage shaderModule) entryPoints

-- https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_vulkan_glsl.txt
-- https://vulkan.lunarg.com/doc/view/1.2.135.0/linux/tutorial/html/08-init_pipeline_layout.html
makeUboDescriptorSetLayoutBinding :: ShaderStage -> Ubo -> (Int, DescriptorSetLayoutBinding)
makeUboDescriptorSetLayoutBinding stage Ubo {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , descriptorCount = maybe 1 (V.sum . (fromIntegral <$>)) array
  , stageFlags = convertStage stage
  })

data TextureDescriptorType
  = Sampler2D

instance Convert TextureDescriptorType where
  from = \case
    "sampler2D" -> Sampler2D
    unsupport -> throw $ ConvertException unsupport $ "TextureDescriptorType not support '" <> unsupport <> "'"
  to = \case
    Sampler2D -> "sampler2D"

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Sampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

makeTextureDescriptorSetLayoutBinding :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = convertTextureDescriptorType . from $ type'
  , descriptorCount = maybe 1 (V.sum . (fromIntegral <$>)) array
  , stageFlags = convertStage stage
  })

makeDescriptorSetLayoutBindings :: ShaderStage -> Vector Ubo -> Vector Texture -> Vector (Int, DescriptorSetLayoutBinding)
makeDescriptorSetLayoutBindings stage ubos textures = uboBindings <> textureBindings
  where
    uboBindings = V.map (makeUboDescriptorSetLayoutBinding stage) ubos
    textureBindings = V.map (makeTextureDescriptorSetLayoutBinding stage) textures

makeDescriptorSetLayoutCreateInfos :: Vector (Int, DescriptorSetLayoutBinding) -> V.Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorSetLayoutCreateInfos bindings = V.map makeDescriptorSetLayoutCreateInfo . V.fromList . M.elems . M.unionWith (V.++) sets $ setsMap
  where
    setLayoutsSize = V.maximum . (fst <$>) $ bindings :: Int
    sets :: Map Int (Vector DescriptorSetLayoutBinding)
    sets = M.fromList . map (, []) $ [ 0 .. setLayoutsSize ]
    setsMap :: Map Int (Vector DescriptorSetLayoutBinding)
    setsMap = M.fromList . map (liftA2 (,) (fst . head) (V.fromList . (snd <$>))) . groupBy ((==) `on` fst) . sortOn fst . V.toList $ bindings

makeDescriptorSetLayoutCreateInfo :: Vector DescriptorSetLayoutBinding -> DescriptorSetLayoutCreateInfo '[]
makeDescriptorSetLayoutCreateInfo bindings = zero { bindings = bindings }

data VertexAttributeType
  = Vec2
  | Vec3
  | Vec4

instance Convert VertexAttributeType where
  from = \case
    "vec2" -> Vec2
    "vec3" -> Vec3
    "vec4" -> Vec4
    unsupport -> throw $ ConvertException unsupport $ "VertexAttributeType not support '" <> unsupport <> "'"
  to = \case
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
makePipelineVertexInputStateCreateInfo inputs = Just zero
  { vertexBindingDescriptions
  , vertexAttributeDescriptions
  }
  where
    vertexAttributes = join . V.map makeVertexAttribute $ inputs :: Vector VertexAttribute
    vertexAttributeDescriptions = makeVertexInputAttributeDescriptions vertexAttributes :: Vector VertexInputAttributeDescription
    vertexBindingDescriptions = makeVertexInputBindingDescriptions vertexAttributes :: Vector VertexInputBindingDescription

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
makeVertexAttribute Input {..} = V.map (\i -> VertexAttribute
  { binding = 0
  , location = fromIntegral . (+ location) $ i
  , size
  , format
  })
  [ 0 .. count - 1
  ]
  where
    count = maybe 1 V.sum array :: Int
    (size, format) = convertVertexAttributeType . from $ type'

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

reflect :: MonadIO m => ShaderStage -> "code" ::: Text -> m (B.ByteString, BL.ByteString)
reflect shaderStage code = liftIO . withSystemTempDirectory "th-spirv" $ \dir -> do
  let stage = T.unpack . to $ shaderStage
  let shader = dir </> "glsl." <> stage
  let spv = dir </> stage <> ".spv"
  T.writeFile shader code
  (exitCode, _spv, err) <- readProcess . proc "glslangValidator" $ [ "-S", stage, "-V", shader, "-o", spv]
  spirv <- B.readFile spv
  (exitCode, reflectionRaw, err) <- readProcess . proc "spirv-cross" $ [spv, "--vulkan-semantics", "--reflect"]
  pure (spirv, reflectionRaw)

reflect' :: MonadIO m => "spirv" ::: B.ByteString -> m BL.ByteString
reflect' spirv = liftIO . withSystemTempDirectory "th-spirv" $ \dir -> do
  let spv = dir </> "glsl.spv"
  B.writeFile spv spirv
  (exitCode, reflectionRaw, err) <- readProcess . proc "spirv-cross" $ [spv, "--vulkan-semantics", "--reflect"]
  pure reflectionRaw

reflection :: MonadIO m => ShaderStage -> "code" ::: Text -> m (Shader, Reflection)
reflection stage code = do
  (spirv, reflectionRaw) <- reflect stage code
  case decode reflectionRaw of
    Just reflection -> pure (Shader {stage, code = spirv}, reflection)
    Nothing -> error "fail to reflect"

reflection' :: MonadIO m => "spirv" ::: B.ByteString -> m (Shader, Reflection)
reflection' spirv = do
  reflectionRaw <- reflect' spirv
  case decode reflectionRaw of
    Just reflection -> pure (Shader {stage = getShaderStage reflection, code = spirv}, reflection)
    Nothing -> error "fail to reflect"

getShaderStage :: Reflection -> ShaderStage
getShaderStage Reflection {..} = mode . V.head $ entryPoints

testVert :: Text
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

testFrag :: Text
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
