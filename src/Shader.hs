{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Shader
  ( ShaderUniform (..)
  , ShaderInputVertex (..)
  , Texture (..)
  )
  where

import GHC.Generics (Generic)
import Linear (V2 (..), V3 (..), M44)
import Foreign.Storable.Generic (GStorable)

import Offset

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderInputVertex

data ShaderUniform = ShaderUniform
  { model :: !(M44 Float)
  , view :: !(M44 Float)
  , proj :: !(M44 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderUniform

data Texture = Texture
  { position :: !(V2 Float)
  , color :: !(V3 Float)
  , texCoord :: !(V2 Float)
  } deriving (Generic, GStorable)

makeOffset ''Texture
