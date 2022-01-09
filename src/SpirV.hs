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
import qualified Data.List.NonEmpty as NE
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

data SeparateImage = SeparateImage
  { type' :: SeparateImageDescriptorType
  , name :: Text
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Show, Generic)

instance FromJSON SeparateImage where
  parseJSON = withObject "separate_images" $ \v -> SeparateImage
    <$> v .: "type"
    <*> v .: "name"
    <*> v .: "set"
    <*> v .: "binding"
    <*> v .:? "array"

instance ToJSON SeparateImage where
  toJSON (SeparateImage type' name set binding array) = object
    [ "type" .= type'
    , "name" .= name
    , "set" .= set
    , "binding" .= binding
    , "array" .= array
    ]

data Reflection = Reflection
  { entryPoints :: Vector EntryPoint
  , inputs :: Maybe (Vector Input)
  , textures :: Maybe (Vector Texture)
  , ubos :: Maybe (Vector Ubo)
  , separate_images :: Maybe (Vector SeparateImage)
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
test0 = Just (Reflection {entryPoints = [EntryPoint {name = "main", mode = Frag}], inputs = Just [Input {type' = Vec2, name = "fragTexCoord", location = 1, array = Nothing},Input {type' = Vec4, name = "fragColor", location = 0, array = Nothing}], textures = Just [Texture {type' = Sampler2D, name = "texSampler", set = 0, binding = 1, array = Just [2]},Texture {type' = Sampler2D, name = "texSampler2", set = 3, binding = 1, array = Nothing}], ubos = Nothing, separate_images = Nothing})

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
    Nothing -> fail $ label <> " unexcept value " <> T.unpack text

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
makeInputInfo = (SomeStruct <$>) . makePipelineVertexInputStateCreateInfo . join . V.mapMaybe (inputs . snd) . V.filter ((== Vert) . (stage :: Shader -> ShaderStage) . fst)

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
  , descriptorCount = maybe 1 (fromIntegral . V.product) array
  , stageFlags = convertStage stage
  })

data TextureDescriptorType
  = Buffer
  | Sampler1D
  | Sampler1DShadow
  | Sampler1DArray
  | Sampler1DArrayShadow
  | Isampler1D
  | Isampler1DArray
  | Usampler1D
  | Usampler1DArray
  | Sampler2D
  | Sampler2DShadow
  | Sampler2DArray
  | Sampler2DArrayShadow
  | Isampler2D
  | Isampler2DArray
  | Usampler2D
  | Usampler2DArray
  | Sampler2DRect
  | Sampler2DRectShadow
  | Isampler2DRect
  | Usampler2DRect
  | Sampler2DMS
  | Isampler2DMS
  | Usampler2DMS
  | Sampler2DMSArray
  | Isampler2DMSArray
  | Usampler2DMSArray
  | Sampler3D
  | Isampler3D
  | Usampler3D
  | SamplerCube
  | SamplerCubeShadow
  | IsamplerCube
  | UsamplerCube
  | SamplerCubeArray
  | SamplerCubeArrayShadow
  | IsamplerCubeArray
  | UsamplerCubeArray
  | SamplerBuffer
  | IsamplerBuffer
  | UsamplerBuffer
  | Image1D
  | Iimage1D
  | Uimage1D
  | Image1DArray
  | Iimage1DArray
  | Uimage1DArray
  | Image2D
  | Iimage2D
  | Uimage2D
  | Image2DArray
  | Iimage2DArray
  | Uimage2DArray
  | Image2DRect
  | Iimage2DRect
  | Uimage2DRect
  | Image2DMS
  | Iimage2DMS
  | Uimage2DMS
  | Image2DMSArray
  | Iimage2DMSArray
  | Uimage2DMSArray  
  | Image3D
  | Iimage3D
  | Uimage3D
  | ImageCube
  | IimageCube
  | UimageCube
  | ImageCubeArray
  | IimageCubeArray
  | UimageCubeArray
  | ImageBuffer
  | IimageBuffer
  | UimageBuffer
  | Texture1D
  | Texture1DArray
  | Itexture1D
  | Itexture1DArray
  | Utexture1D
  | Utexture1DArray
  | Texture2D
  | Texture2DArray
  | Itexture2D
  | Itexture2DArray
  | Utexture2D
  | Utexture2DArray
  | Texture2DRect
  | Itexture2DRect
  | Utexture2DRect
  | Texture2DMS
  | Itexture2DMS
  | Utexture2DMS
  | Texture2DMSArray
  | Itexture2DMSArray
  | Utexture2DMSArray
  | Texture3D
  | Itexture3D
  | Utexture3D
  | TextureCube
  | ItextureCube
  | UtextureCube
  | TextureCubeArray
  | ItextureCubeArray
  | UtextureCubeArray
  | Sampler
  | SamplerShadow
  | SubpassInput
  | IsubpassInput
  | UsubpassInput
  | SubpassInputMS
  | IsubpassInputMS
  | UsubpassInputMS
  deriving (Show)

instance Convert TextureDescriptorType where
  maybeFrom = \case
    "buffer" -> Just Buffer
    "sampler1D" -> Just Sampler1D
    "sampler1DShadow" -> Just Sampler1DShadow
    "sampler1DArray" -> Just Sampler1DArray
    "sampler1DArrayShadow" -> Just Sampler1DArrayShadow
    "isampler1D" -> Just Isampler1D
    "isampler1DArray" -> Just Isampler1DArray
    "usampler1D" -> Just Usampler1D
    "usampler1DArray" -> Just Usampler1DArray
    "sampler2D" -> Just Sampler2D
    "sampler2DShadow" -> Just Sampler2DShadow
    "sampler2DArray" -> Just Sampler2DArray
    "sampler2DArrayShadow" -> Just Sampler2DArrayShadow
    "isampler2D" -> Just Isampler2D
    "isampler2DArray" -> Just Isampler2DArray
    "usampler2D" -> Just Usampler2D
    "usampler2DArray" -> Just Usampler2DArray
    "sampler2DRect" -> Just Sampler2DRect
    "sampler2DRectShadow" -> Just Sampler2DRectShadow
    "isampler2DRect" -> Just Isampler2DRect
    "usampler2DRect" -> Just Usampler2DRect
    "sampler2DMS" -> Just Sampler2DMS
    "isampler2DMS" -> Just Isampler2DMS
    "usampler2DMS" -> Just Usampler2DMS
    "sampler2DMSArray" -> Just Sampler2DMSArray
    "isampler2DMSArray" -> Just Isampler2DMSArray
    "usampler2DMSArray" -> Just Usampler2DMSArray
    "sampler3D" -> Just Sampler3D
    "isampler3D" -> Just Isampler3D
    "usampler3D" -> Just Usampler3D
    "samplerCube" -> Just SamplerCube
    "samplerCubeShadow" -> Just SamplerCubeShadow
    "isamplerCube" -> Just IsamplerCube
    "usamplerCube" -> Just UsamplerCube
    "samplerCubeArray" -> Just SamplerCubeArray
    "samplerCubeArrayShadow" -> Just SamplerCubeArrayShadow
    "isamplerCubeArray" -> Just IsamplerCubeArray
    "usamplerCubeArray" -> Just UsamplerCubeArray
    "samplerBuffer" -> Just SamplerBuffer
    "isamplerBuffer" -> Just IsamplerBuffer
    "usamplerBuffer" -> Just UsamplerBuffer
    "image1D" -> Just Image1D
    "iimage1D" -> Just Iimage1D
    "uimage1D" -> Just Uimage1D
    "image1DArray" -> Just Image1DArray
    "iimage1DArray" -> Just Iimage1DArray
    "uimage1DArray" -> Just Uimage1DArray
    "image2D" -> Just Image2D
    "iimage2D" -> Just Iimage2D
    "uimage2D" -> Just Uimage2D
    "image2DArray" -> Just Image2DArray
    "iimage2DArray" -> Just Iimage2DArray
    "uimage2DArray" -> Just Uimage2DArray
    "image2DRect" -> Just Image2DRect
    "iimage2DRect" -> Just Iimage2DRect
    "uimage2DRect" -> Just Uimage2DRect
    "image2DMS" -> Just Image2DMS
    "iimage2DMS" -> Just Iimage2DMS
    "uimage2DMS" -> Just Uimage2DMS
    "image2DMSArray" -> Just Image2DMSArray
    "iimage2DMSArray" -> Just Iimage2DMSArray
    "uimage2DMSArray" -> Just Uimage2DMSArray
    "image3D" -> Just Image3D
    "Iimage3D" -> Just Iimage3D
    "Uimage3D" -> Just Uimage3D
    "imageCube" -> Just ImageCube
    "iimageCube" -> Just IimageCube
    "uimageCube" -> Just UimageCube
    "imageCubeArray" -> Just ImageCubeArray
    "iimageCubeArray" -> Just IimageCubeArray
    "uimageCubeArray" -> Just UimageCubeArray
    "imageBuffer" -> Just ImageBuffer
    "iimageBuffer" -> Just IimageBuffer
    "uimageBuffer" -> Just UimageBuffer
    "texture1D" -> Just Texture1D
    "texture1DArray" -> Just Texture1DArray
    "itexture1D" -> Just Itexture1D
    "itexture1DArray" -> Just Itexture1DArray
    "utexture1D" -> Just Utexture1D
    "utexture1DArray" -> Just Utexture1DArray
    "texture2D" -> Just Texture2D
    "texture2DArray" -> Just Texture2DArray
    "itexture2D" -> Just Itexture2D
    "itexture2DArray" -> Just Itexture2DArray
    "utexture2D" -> Just Utexture2D
    "utexture2DArray" -> Just Utexture2DArray
    "texture2DRect" -> Just Texture2DRect
    "itexture2DRect" -> Just Itexture2DRect
    "utexture2DRect" -> Just Utexture2DRect
    "texture2DMS" -> Just Texture2DMS
    "itexture2DMS" -> Just Itexture2DMS
    "utexture2DMS" -> Just Utexture2DMS
    "texture2DMSArray" -> Just Texture2DMSArray
    "itexture2DMSArray" -> Just Itexture2DMSArray
    "utexture2DMSArray" -> Just Utexture2DMSArray
    "texture3D" -> Just Texture3D
    "itexture3D" -> Just Itexture3D
    "utexture3D" -> Just Utexture3D
    "textureCube" -> Just TextureCube
    "itextureCube" -> Just ItextureCube
    "utextureCube" -> Just UtextureCube
    "textureCubeArray" -> Just TextureCubeArray
    "itextureCubeArray" -> Just ItextureCubeArray
    "utextureCubeArray" -> Just UtextureCubeArray
    -- "textureBuffer" -> Just TextureBuffer
    -- "itextureBuffer" -> Just ItextureBuffer
    -- "utextureBuffer" -> Just UtextureBuffer
    "sampler" -> Just Sampler
    "samplerShadow" -> Just SamplerShadow
    "subpassInput" -> Just SubpassInput
    "isubpassInput" -> Just IsubpassInput
    "usubpassInput" -> Just UsubpassInput
    "subpassInputMS" -> Just SubpassInputMS
    "IsubpassInputMS" -> Just IsubpassInputMS
    "UsubpassInputMS" -> Just UsubpassInputMS
    _ -> Nothing
  to = \case
    Buffer -> "buffer"
    Sampler1D -> "sampler1D"
    Sampler1DShadow -> "sampler1DShadow"
    Sampler1DArray -> "sampler1DArray"
    Sampler1DArrayShadow -> "sampler1DArrayShadow"
    Isampler1D -> "isampler1D"
    Isampler1DArray -> "isampler1DArray"
    Usampler1D -> "usampler1D"
    Usampler1DArray -> "usampler1DArray"
    Sampler2D -> "sampler2D"
    Sampler2DShadow -> "sampler2DShadow"
    Sampler2DArray -> "sampler2DArray"
    Sampler2DArrayShadow -> "sampler2DArrayShadow"
    Isampler2D -> "isampler2D"
    Isampler2DArray -> "isampler2DArray"
    Usampler2D -> "usampler2D"
    Usampler2DArray -> "usampler2DArray"
    Sampler2DRect -> "sampler2DRect"
    Sampler2DRectShadow -> "sampler2DRectShadow"
    Isampler2DRect -> "isampler2DRect"
    Usampler2DRect -> "usampler2DRect"
    Sampler2DMS -> "sampler2DMS"
    Isampler2DMS -> "isampler2DMS"
    Usampler2DMS -> "usampler2DMS"
    Sampler2DMSArray -> "sampler2DMSArray"
    Isampler2DMSArray -> "isampler2DMSArray"
    Usampler2DMSArray -> "usampler2DMSArray"
    Sampler3D -> "sampler3D"
    Isampler3D -> "isampler3D"
    Usampler3D -> "usampler3D"
    SamplerCube -> "samplerCube"
    SamplerCubeShadow -> "samplerCubeShadow"
    IsamplerCube -> "isamplerCube"
    UsamplerCube -> "usamplerCube"
    SamplerCubeArray -> "samplerCubeArray"
    SamplerCubeArrayShadow -> "samplerCubeArrayShadow"
    IsamplerCubeArray -> "isamplerCubeArray"
    UsamplerCubeArray -> "usamplerCubeArray"
    SamplerBuffer -> "samplerBuffer"
    IsamplerBuffer -> "isamplerBuffer"
    UsamplerBuffer -> "usamplerBuffer"
    Image1D -> "image1D"
    Iimage1D -> "iimage1D"
    Uimage1D -> "uimage1D"
    Image1DArray -> "image1DArray"
    Iimage1DArray -> "iimage1DArray"
    Uimage1DArray -> "uimage1DArray"
    Image2D -> "image2D"
    Iimage2D -> "iimage2D"
    Uimage2D -> "uimage2D"
    Image2DArray -> "image2DArray"
    Iimage2DArray -> "iimage2DArray"
    Uimage2DArray -> "uimage2DArray"
    Image2DRect -> "image2DRect"
    Iimage2DRect -> "iimage2DRect"
    Uimage2DRect -> "uimage2DRect"
    Image2DMS -> "image2DMS"
    Iimage2DMS -> "iimage2DMS"
    Uimage2DMS -> "uimage2DMS"
    Image2DMSArray -> "image2DMSArray"
    Iimage2DMSArray -> "iimage2DMSArray"
    Uimage2DMSArray -> "uimage2DMSArray"
    Image3D -> "image3D"
    Iimage3D -> "iimage3D"
    Uimage3D -> "uimage3D"
    ImageCube -> "imageCube"
    IimageCube -> "iimageCube"
    UimageCube -> "uimageCube"
    ImageCubeArray -> "imageCubeArray"
    IimageCubeArray -> "iimageCubeArray"
    UimageCubeArray -> "uimageCubeArray"
    ImageBuffer -> "imageBuffer"
    IimageBuffer -> "iimageBuffer"
    UimageBuffer -> "uimageBuffer"
    Texture1D -> "texture1D"
    Texture1DArray -> "texture1DArray"
    Itexture1D -> "itexture1D"
    Itexture1DArray -> "itexture1DArray"
    Utexture1D -> "utexture1D"
    Utexture1DArray -> "utexture1DArray"
    Texture2D -> "texture2DArray"
    Texture2DArray -> "texture2DArray"
    Itexture2D -> "itexture2D"
    Itexture2DArray -> "itexture2DArray"
    Utexture2D -> "utexture2D"
    Utexture2DArray -> "utexture2DArray"
    Texture2DRect -> "texture2DRect"
    Itexture2DRect -> "itexture2DRect"
    Utexture2DRect -> "utexture2DRect"
    Texture2DMS -> "texture2DMS"
    Itexture2DMS -> "itexture2DMS"
    Utexture2DMS -> "utexture2DMS"
    Texture2DMSArray -> "texture2DMSArray"
    Itexture2DMSArray -> "itexture2DMSArray"
    Utexture2DMSArray -> "utexture2DMSArray"
    Texture3D -> "texture3D"
    Itexture3D -> "itexture3D"
    Utexture3D -> "utexture3D"
    TextureCube -> "textureCube"
    ItextureCube -> "itextureCube"
    UtextureCube -> "utextureCube"
    TextureCubeArray -> "textureCubeArray"
    ItextureCubeArray -> "itextureCubeArray"
    UtextureCubeArray -> "utextureCubeArray"
    -- TextureBuffer -> "textureBuffer"
    -- ItextureBuffer -> "itextureBuffer"
    -- UtextureBuffer -> "utextureBuffer"
    Sampler -> "sampler"
    SamplerShadow -> "samplerShadow"
    SubpassInput -> "subpassInput"
    IsubpassInput -> "isubpassInput"
    UsubpassInput -> "usubpassInput"
    SubpassInputMS -> "subpassInputMS"
    IsubpassInputMS -> "isubpassInputMS"
    UsubpassInputMS -> "usubpassInputMS"

instance FromJSON TextureDescriptorType where
  parseJSON = withTextMaybe "type" (maybeFrom @TextureDescriptorType)

instance ToJSON TextureDescriptorType where
  toJSON = String . to
  toEncoding = toEncoding . to

convertTextureDescriptorType :: TextureDescriptorType -> DescriptorType
convertTextureDescriptorType = \case
  Buffer -> DESCRIPTOR_TYPE_STORAGE_BUFFER
  Sampler1D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler1DArrayShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler1D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler1DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler1D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler1DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DArrayShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DRect -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DRectShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DRect -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DRect -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DMS -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DMS -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DMS -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler2DMSArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler2DMSArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler2DMSArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Sampler3D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Isampler3D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  Usampler3D -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCube -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  IsamplerCube -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  UsamplerCube -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerCubeArrayShadow -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  IsamplerCubeArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  UsamplerCubeArray -> DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SamplerBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  IsamplerBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  UsamplerBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  Image1D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage1D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage1D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image1DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage1DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage1DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DRect -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DRect -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DRect -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DMS -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DMS -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DMS -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image2DMSArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage2DMSArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage2DMSArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Image3D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Iimage3D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  Uimage3D -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageCube -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  IimageCube -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  UimageCube -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageCubeArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  IimageCubeArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  UimageCubeArray -> DESCRIPTOR_TYPE_STORAGE_IMAGE
  ImageBuffer -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  IimageBuffer -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  UimageBuffer -> DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  Texture1D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture1DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture1D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture1DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture1D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture1DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DRect -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DRect -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DRect -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DMS -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DMS -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DMS -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture2DMSArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture2DMSArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture2DMSArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Texture3D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Itexture3D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  Utexture3D -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  TextureCube -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  ItextureCube -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  UtextureCube -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  TextureCubeArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  ItextureCubeArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  UtextureCubeArray -> DESCRIPTOR_TYPE_SAMPLED_IMAGE
  -- TextureBuffer -> undefined
  -- ItextureBuffer -> undefined
  -- UtextureBuffer -> undefined
  Sampler -> DESCRIPTOR_TYPE_SAMPLER
  SamplerShadow -> DESCRIPTOR_TYPE_SAMPLER
  SubpassInput -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  IsubpassInput -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  UsubpassInput -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  SubpassInputMS -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  IsubpassInputMS -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  UsubpassInputMS -> DESCRIPTOR_TYPE_INPUT_ATTACHMENT

data SeparateImageDescriptorType
  = TextureBuffer
  | ItextureBuffer
  | UtextureBuffer
  deriving (Show)

instance Convert SeparateImageDescriptorType where
  maybeFrom = \case
    "samplerBuffer" -> Just TextureBuffer
    "isamplerBuffer" -> Just ItextureBuffer
    "utextureBuffer" -> Just UtextureBuffer
    _ -> Nothing
  to = \case
    TextureBuffer -> "samplerBuffer"
    ItextureBuffer -> "isamplerBuffer"
    UtextureBuffer -> "utextureBuffer"

instance FromJSON SeparateImageDescriptorType where
  parseJSON = withTextMaybe "type" (maybeFrom @SeparateImageDescriptorType)

instance ToJSON SeparateImageDescriptorType where
  toJSON = String . to
  toEncoding = toEncoding . to

convertSeparateImageDescriptorType :: SeparateImageDescriptorType -> DescriptorType
convertSeparateImageDescriptorType = \case
  TextureBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  ItextureBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  UtextureBuffer -> DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER

makeTextureDescriptorSetLayoutBinding :: ShaderStage -> Texture -> (Int, DescriptorSetLayoutBinding)
makeTextureDescriptorSetLayoutBinding stage Texture {..} = (set, zero
  { binding = fromIntegral binding
  , descriptorType = convertTextureDescriptorType type'
  , descriptorCount = maybe 1 (fromIntegral . V.product) array
  , stageFlags = convertStage stage
  })

makeDescriptorSetLayoutBindings :: ShaderStage -> Vector Ubo -> Vector Texture -> Vector (Int, DescriptorSetLayoutBinding)
makeDescriptorSetLayoutBindings stage ubos textures = uboBindings <> textureBindings
  where
    uboBindings = V.map (makeUboDescriptorSetLayoutBinding stage) ubos
    textureBindings = V.map (makeTextureDescriptorSetLayoutBinding stage) textures

makeDescriptorSetLayoutCreateInfos :: Vector (Int, DescriptorSetLayoutBinding) -> V.Vector (DescriptorSetLayoutCreateInfo '[])
makeDescriptorSetLayoutCreateInfos bindings = V.map makeDescriptorSetLayoutCreateInfo . V.fromList . M.elems . M.unionWith (V.++) emptySetsMap $ setsMap
  where
    setLayoutsSize = V.maximum . fmap fst $ bindings :: Int
    emptySetsMap :: Map Int (Vector DescriptorSetLayoutBinding)
    emptySetsMap = M.fromList . map (, []) $ [ 0 .. setLayoutsSize ]
    setsMap :: Map Int (Vector DescriptorSetLayoutBinding)
    setsMap = M.fromList . map extract . NE.groupAllWith fst . V.toList $ bindings
    extract :: NE.NonEmpty (Int, DescriptorSetLayoutBinding) -> (Int, Vector DescriptorSetLayoutBinding)
    extract = liftA2 (,) (fst . NE.head) (V.fromList . NE.toList . fmap snd)

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
makeVertexInputBindingDescriptions = V.fromList . map (makeVertexInputBindingDescription . calculate) . NE.groupAllWith fst . map extract . V.toList
-- makeVertexInputBindingDescriptions = V.fromList . map (makeVertexInputBindingDescription . calculate) . groupBy ((==) `on` fst) . sortOn fst . map extract . V.toList
  where
    extract :: VertexAttribute -> ("binding" ::: Int, "size" ::: Int)
    extract = liftA2 (,) (fromIntegral . (binding :: VertexAttribute -> Word32)) (fromIntegral . size)
    calculate :: NE.NonEmpty ("binding" ::: Int, "size" ::: Int) -> ("binding" ::: Int, "stride" ::: Int)
    calculate = liftA2 (,) (fst . NE.head) (sum . (snd <$>))

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
    count = maybe 1 V.product array :: Int
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
  case eitherDecode reflectionRaw of
    Right reflection -> pure (Shader {stage, code = spirv}, reflection)
    Left err -> error $ "fail to reflect: " <> err

eitherReflection  :: MonadIO m => ShaderStage -> "code" ::: Text -> m (Either String (Shader, Reflection))
eitherReflection stage code = runExceptT $ do
  (spirv, reflectionRaw) <- except =<< eitherReflect stage code
  ref <- except . eitherDecodeStrict' @Reflection . BL.toStrict $ reflectionRaw
  pure (Shader { stage, code = spirv }, ref)

reflection' :: MonadIO m => "spirv" ::: B.ByteString -> m (Shader, Reflection)
reflection' spirv = do
  reflectionRaw <- reflect' spirv
  case eitherDecode reflectionRaw of
    Right reflection -> pure (Shader {stage = getShaderStage reflection, code = spirv}, reflection)
    Left err -> error $ "fail to reflect: " <> err

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
