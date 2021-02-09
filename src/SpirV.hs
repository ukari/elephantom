{-# LANGUAGE QuasiQuotes #-}

module SpirV
  (
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Aeson --(decode, eitherDecode)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcess)
import System.IO.Temp (withSystemTempDirectory)
import Text.InterpolatedString.QM (qnb)
import Control.Monad.IO.Class (MonadIO, liftIO)

test :: MonadIO m => m (Maybe Value)
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
