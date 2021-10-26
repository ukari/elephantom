{-# LANGUAGE GADTs #-}

{-# LANGUAGE OverloadedStrings #-}

module Language.GLSL.Compilation
  ( GLSLC (..)
  , Version (..)
  , Directive (..)
  , SourceCode (..)
  , parse
  ) where


import Text.Parsec.Prim hiding (parse)
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Error

import qualified Vulkan.Utils.ShaderQQ.GLSL.Glslang as VulkanUtils

import Data.Text (Text, pack, unpack)

import Data.Functor (void)
-- import Control.Monad.Trans.Except (ExceptT, runExcept, withExcept)
import Control.Exception (Exception)

-- https://www.khronos.org/opengl/wiki/Core_Language_(GLSL)
-- https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GL_KHR_vulkan_glsl.txt

-- not support #define, #ifdef, #if, #else, #endif
newtype Version = Version Text deriving (Show)

data Directive
  = Extension Text
  | Line Text
  | Pragma Text
  deriving (Show)

newtype SourceCode = SourceCode Text deriving (Show)

data GLSLC = GLSLC
  { version :: !Version
  , directives :: ![ Directive ]
  , sourceCode :: !SourceCode
  } deriving (Show)

versionParser :: Parser Version
versionParser = do
  void . many $ newline
  void . string $ "#version"
  void spaces
  v <- manyTill anyChar endOfLine
  pure . Version . pack $ v

extensionParser :: Parser Directive
extensionParser = do
  void . many $ newline
  void . string $ "#extension"
  void spaces
  ext <- manyTill anyChar endOfLine
  pure . Extension . pack $ ext

lineParser :: Parser Directive
lineParser = do
  void . many $ newline
  void . string $ "#line"
  void spaces
  line <- manyTill anyChar endOfLine
  pure . Line . pack $ line

paramParser :: Parser Directive
paramParser = do
  void . many $ newline
  void . string $ "#pragma"
  void spaces
  pargma <- manyTill anyChar endOfLine
  pure . Pragma . pack $ pargma

sourceCodeParser :: Parser SourceCode
sourceCodeParser = do
  s <- manyTill anyChar eof
  pure . SourceCode . pack $ s

glslParser :: Parser GLSLC
glslParser = do
  v <- versionParser
  ds <- many . choice $ try <$>
    [ extensionParser
    , lineParser
    , paramParser
    ]
  GLSLC v ds <$> sourceCodeParser

data GLSLException
  = PreprocessException String
  deriving (Show)

instance Exception GLSLException

parse :: Text -> Either ParseError GLSLC
parse = Parsec.parse glslParser ""
