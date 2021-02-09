{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SpirV
  (
  ) where


import GHC.Generics
import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Aeson --(decode, eitherDecode)
import Data.Vector (Vector)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess)
import System.IO.Temp (withSystemTempDirectory)
import Text.InterpolatedString.QM (qnb)
import Control.Monad.IO.Class (MonadIO, liftIO)

data EntryPoint = EntryPoint
  { name :: String
  , mode :: String
  } deriving (Generic, Show, FromJSON, ToJSON)

data Input = Input
  { type' :: String
  , name :: String
  , location :: Int
  , array :: Maybe (Vector Int)
  } deriving (Generic, Show)

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

data UBO = UBO
  { set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Generic, Show, FromJSON, ToJSON)

data Texture = Texture
  { type' :: String
  , name :: String
  , set :: Int
  , binding :: Int
  , array :: Maybe (Vector Int)
  } deriving (Generic, Show)

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
  , inputs :: Vector Input
  , textures :: Vector Texture
  , ubos :: Vector UBO
  } deriving (Generic, Show, FromJSON, ToJSON)

test :: MonadIO m => m (Maybe Reflection)
test = decode <$> reflect testCode

reflect :: MonadIO m => String -> m ByteString
reflect code = liftIO . withSystemTempDirectory "th-spirv" $ \dir -> do
  let shader = dir </> "test.vert"
  let spv = dir </> "vert.spv"
  writeFile shader code
  writeFile spv code
  (exitCode, out, err) <- readProcess . proc "glslangValidator" $ [ "-S", "vert", "-V", shader, "-o", spv]
  --print out
  (exitCode, out, err) <- readProcess . proc "spirv-cross" $ [spv, "--vulkan-semantics", "--reflect"]
  pure out

testCode :: String
testCode = [qnb|
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
