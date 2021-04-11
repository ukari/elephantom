{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Aeson ((.=), (.:), (.:?), FromJSON (..), ToJSON (..), Value (String), decode, decode', encode, eitherDecode, eitherDecodeStrict', object, pairs, withObject, withText, genericToEncoding, defaultOptions)
import Data.Aeson.Types (prependFailure, typeMismatch, unexpected, parseMaybe, Parser, parseFail)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.FilePath ((</>), (<.>))
import System.Process.Typed (proc, readProcess)
import GHC.IO.Exception (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import Text.InterpolatedString.QM (qnb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List ((\\), group, sort, sortOn, groupBy, mapAccumL, foldl')
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
import Data.Function (on)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word32)
import Data.Either (fromRight)
import Control.Applicative (liftA2)
import Control.Monad (join, ap)
import Control.Exception (Exception (..), SomeException (..), throw, catch, handle, evaluate)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, except)
import Debug.Trace (traceShow)

data EntryPoint = EntryPoint
  { name :: Text
  , mode :: ShaderStage
  } deriving (Show, Generic, FromJSON, ToJSON)

data Input = Input
  { type' :: VertexAttributeType
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
  -- toEncoding (Input type' name location array) = pairs
  --   (  "type" .= type'
  --   <> "name" .= name
  --   <> "location" .= location
  --   <> "array" .= array
  --   )

data Ubo = Ubo
  { name :: Text
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic, FromJSON, ToJSON)

data Texture = Texture
  { type' :: TextureDescriptorType
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
  -- toEncoding (Texture type' name set binding array) = pairs
  --   (  "type" .= type'
  --   <> "name" .= name
  --   <> "set" .= set
  --   <> "binding" .= binding
  --   <> "array" .= array
  --   )

data Reflection = Reflection
  { entryPoints :: Vector EntryPoint
  , inputs :: Maybe (Vector Input)
  , textures :: Maybe (Vector Texture)
  , ubos :: Maybe (Vector Ubo)
  } deriving (Show, Generic, FromJSON, ToJSON)

test :: MonadIO m => m (Maybe Reflection)
test = decode . snd <$> reflect (from "vert") testVert

eitherTest :: MonadIO m => m (Maybe Reflection)
eitherTest = do
  eres <- eitherReflect (from "vert") testVert
  case eres of
    Right reflection -> pure . decode . snd $ reflection
    Left err -> error err

testInput = Input {type' = Vec2, name = "fragTexCoord", location = 1, array = Nothing}

test0 :: Maybe Reflection
test0 = Just (Reflection {entryPoints = [EntryPoint {name = "main", mode = Frag}], inputs = Just [Input {type' = Vec2, name = "fragTexCoord", location = 1, array = Nothing},Input {type' = Vec4, name = "fragColor", location = 0, array = Nothing}], textures = Just [Texture {type' = Sampler2D, name = "texSampler", set = 0, binding = 1, array = Just [2]},Texture {type' = Sampler2D, name = "texSampler2", set = 3, binding = 1, array = Nothing}], ubos = Nothing})

test0Encode :: BL.ByteString
test0Encode = "{\"textures\":[{\"set\":0,\"array\":[2],\"name\":\"texSampler\",\"type\":\"sampler2D\",\"binding\":1},{\"set\":3,\"array\":null,\"name\":\"texSampler2\",\"type\":\"sampler2D\",\"binding\":1}],\"inputs\":[{\"location\":1,\"array\":null,\"name\":\"fragTexCoord\",\"type\":\"vec2\"},{\"location\":0,\"array\":null,\"name\":\"fragColor\",\"type\":\"vec4\"}],\"ubos\":null,\"entryPoints\":[{\"mode\":\"frag\",\"name\":\"main\"}]}"

test1 :: MonadIO m => m (Maybe Reflection)
test1 = decode . snd <$> reflect (from "frag") testFrag

test2 :: MonadIO m => m ()
test2 = do
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

data ConvertException = ConvertException Text
  deriving (Show)

instance Exception ConvertException

class Convert a where
  maybeFrom :: Text -> Maybe a
  from :: Text -> a
  from x = fromMaybe (throw $ ConvertException $ "unsupport " <> x) (maybeFrom x)
  to :: a -> Text

withTextMaybe :: String -> (Text -> Maybe a) -> Value -> Parser a
withTextMaybe label converter = withText label $ \text ->
  case converter text of
    Just res -> pure res
    Nothing -> parseFail $ label <> " unexcept value " <> T.unpack text

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
  maybeFrom = \case
    "vert" -> Just Vert
    "frag" -> Just Frag
    "comp" -> Just Comp
    "tesc" -> Just Tesc
    "tese" -> Just Tese
    "geom" -> Just Geom
    "rgen" -> Just Rgen
    "rint" -> Just Rint
    "rahit" -> Just Rahit
    "rchit" -> Just Rchit
    "rmiss" -> Just Rmiss
    "rcall" -> Just Rcall
    "task" -> Just Task
    "mesh" -> Just Mesh
    _ -> Nothing
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
  parseJSON = withTextMaybe "mode" (maybeFrom @ShaderStage)

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
makeDescriptorInfo xs = makeDescriptorSetLayoutCreateInfos $ do
  (Shader{stage}, Reflection{ubos, textures}) <- xs
  makeDescriptorSetLayoutBindings stage (fromMaybe [] ubos) (fromMaybe [] textures)
-- makeDescriptorInfo = makeDescriptorSetLayoutCreateInfos . join . V.map (makeDescriptorSetLayoutBindings . (stage :: Shader -> ShaderStage) . fst <*> fromMaybe [] . ubos . snd <*> fromMaybe [] . textures . snd)

makeInputInfo :: Vector (Shader, Reflection) -> Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
makeInputInfo = (SomeStruct <$>) . makePipelineVertexInputStateCreateInfo . join . V.mapMaybe (inputs .snd) . V.filter ((== Vert) . (stage :: Shader -> ShaderStage) . fst)

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
  , descriptorCount = maybe 1 (fromIntegral . V.sum) array
  , stageFlags = convertStage stage
  })

data TextureDescriptorType
  = Sampler2D
  deriving (Show)

instance Convert TextureDescriptorType where
  maybeFrom = \case
    "sampler2D" -> Just Sampler2D
    _ -> Nothing
  to = \case
    Sampler2D -> "sampler2D"

instance FromJSON TextureDescriptorType where
  parseJSON = withTextMaybe "type" (maybeFrom @TextureDescriptorType)

instance ToJSON TextureDescriptorType where
  toJSON = String . to
  toEncoding = toEncoding . to

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Sampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER

makeTextureDescriptorSetLayoutBinding :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = convertTextureDescriptorType type'
  , descriptorCount = maybe 1 (fromIntegral . V.sum) array
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
  deriving (Show)

instance Convert VertexAttributeType where
  maybeFrom = \case
    "vec2" -> Just Vec2
    "vec3" -> Just Vec3
    "vec4" -> Just Vec4
    _ -> Nothing
  to = \case
    Vec2 -> "vec2"
    Vec3 -> "vec3"
    Vec4 -> "vec4"

instance FromJSON VertexAttributeType where
  parseJSON = withTextMaybe "type" (maybeFrom @VertexAttributeType)

instance ToJSON VertexAttributeType where
  toJSON = String . to
  toEncoding = toEncoding @Text . to

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
makeVertexInputBindingDescriptions = V.fromList . map (makeVertexInputBindingDescription . calculate) . groupBy ((==) `on` fst) . sortOn fst . map extract . V.toList
  where
    extract :: VertexAttribute -> ("binding" ::: Int, "size" ::: Int)
    extract = liftA2 (,) (fromIntegral . (binding :: VertexAttribute -> Word32)) (fromIntegral . size)
    calculate :: [ ("binding" ::: Int, "size" ::: Int) ]  -> ("binding" ::: Int, "stride" ::: Int) 
    calculate = liftA2 (,) (fst . head) (sum . (snd <$>))

makeVertexInputBindingDescription :: ("binding" ::: Int, "stride" ::: Int) -> VertexInputBindingDescription
makeVertexInputBindingDescription (binding, stride) = zero
  { binding = fromIntegral binding
  , stride = fromIntegral stride
  , inputRate = VERTEX_INPUT_RATE_VERTEX
  }

makeVertexAttribute :: Input -> Vector VertexAttribute
makeVertexAttribute Input {..} = V.generate count (\i -> VertexAttribute
  { binding = 0
  , location = fromIntegral $ location + i
  , size
  , format
  })
  where
    count = maybe 1 V.sum array :: Int
    (size, format) = convertVertexAttributeType type'

makeVertexInputAttributeDescriptions :: Vector VertexAttribute -> Vector VertexInputAttributeDescription
makeVertexInputAttributeDescriptions = V.fromList . join . map process . groupBy ((==) `on` (binding :: VertexAttribute -> Word32)) . V.toList
  where
    process :: [ VertexAttribute ] -> [ VertexInputAttributeDescription ]
    process = snd . mapAccumL makeVertexInputAttributeDescription 0

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
  let shader = dir </> stage <.> "glsl"
  let spv = dir </> stage <.> "spv"
  T.writeFile shader code
  (exitCode, _spv, err) <- readProcess . proc "glslangValidator" $ [ "-S", stage, "-V", shader, "-o", spv ]
  spirv <- B.readFile spv
  (exitCode, reflectionRaw, err) <- readProcess . proc "spirv-cross" $ [ spv, "--vulkan-semantics", "--reflect" ]
  pure (spirv, reflectionRaw)

readProcessHandler :: MonadIO m => (ExitCode, BL.ByteString, BL.ByteString) -> ExceptT String m BL.ByteString
readProcessHandler (exitCode, result, err) = case exitCode of
    ExitFailure errCode -> throwE $ "errCode: " <> show errCode <> ", " <> BL.unpack (if BL.null err then result else err)
    ExitSuccess -> pure result

procE :: MonadIO m => FilePath -> [String] -> ExceptT String m BL.ByteString
procE = fmap ((readProcessHandler =<<) . readProcess) . proc

eitherReflect :: MonadIO m => ShaderStage -> "code" ::: Text -> m (Either String (B.ByteString, BL.ByteString))
eitherReflect shaderStage code = liftIO . runExceptT .  withSystemTempDirectory "th-spirv" $ \dir -> do
  let stage = T.unpack . to $ shaderStage
  let shader = dir </> stage <.> "glsl"
  let spv = dir </> stage <.> "spv"
  liftIO . T.writeFile shader $ code
  _spv <- procE "glslangValidator" [ "-S", stage, "-V", shader, "-o", spv ]
  spirv <- liftIO . B.readFile $ spv
  reflectionRaw <- procE "spirv-cross" [ spv, "--vulkan-semantics", "--reflect" ]
  pure (spirv, reflectionRaw)

reflect' :: MonadIO m => "spirv" ::: B.ByteString -> m BL.ByteString
reflect' spirv = liftIO . withSystemTempDirectory "th-spirv" $ \dir -> do
  let spv = dir </> "shader" <.> "spv"
  B.writeFile spv spirv
  (exitCode, reflectionRaw, err) <- readProcess . proc "spirv-cross" $ [ spv, "--vulkan-semantics", "--reflect" ]
  pure reflectionRaw

eitherReflect' :: MonadIO m => "spirv" ::: B.ByteString -> m (Either String BL.ByteString)
eitherReflect' spirv = liftIO . runExceptT . withSystemTempDirectory "th-spirv" $ \dir -> do
  let spv = dir </> "shader" <.> "spv"
  liftIO . B.writeFile spv $ spirv
  procE "spirv-cross" [ spv, "--vulkan-semantics", "--reflect" ]

reflection :: MonadIO m => ShaderStage -> "code" ::: Text -> m (Shader, Reflection)
reflection stage code = do
  (spirv, reflectionRaw) <- reflect stage code
  case decode reflectionRaw of
    Just reflection -> pure (Shader {stage, code = spirv}, reflection)
    Nothing -> error "fail to reflect"

eitherReflection  :: MonadIO m => ShaderStage -> "code" ::: Text -> m (Either String (Shader, Reflection))
eitherReflection stage code = runExceptT $ do
  (spirv, reflectionRaw) <- except =<< eitherReflect stage code
  ref <- except . eitherDecodeStrict' @Reflection . BL.toStrict $ reflectionRaw
  pure (Shader { stage, code = spirv }, ref)
  
reflection' :: MonadIO m => "spirv" ::: B.ByteString -> m (Shader, Reflection)
reflection' spirv = do
  reflectionRaw <- reflect' spirv
  case decode reflectionRaw of
    Just reflection -> pure (Shader {stage = getShaderStage reflection, code = spirv}, reflection)
    Nothing -> error "fail to reflect"

eitherReflection' :: MonadIO m => "spirv" ::: B.ByteString -> m (Either String (Shader, Reflection))
eitherReflection' spirv = runExceptT $ do
  reflectionRaw <- except =<< eitherReflect' spirv
  ref <- except . eitherDecodeStrict' @Reflection . BL.toStrict $ reflectionRaw
  pure (Shader { stage = getShaderStage ref, code = spirv }, ref) 

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

testHlslFrag :: Text
testHlslFrag = [qnb|
  float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
  vertCode = [vert|
        const static float2 positions[3] = {
          {0.0, -0.5},
          {0.5, 0.5},
          {-0.5, 0.5}
        };

        const static float3 colors[3] = {
          {1.0, 1.0, 0.0},
          {0.0, 1.0, 1.0},
          {1.0, 0.0, 1.0}
        };

        struct VSOutput
        {
          float4 pos : SV_POSITION;
          [[vk::location(0)]] float3 col;
        };

        VSOutput main(const uint i : SV_VertexID)
        {
          VSOutput output;
          output.pos = float4(positions[i], 0, 1.0);
          output.col = colors[i];
          return output;
        }
|]
