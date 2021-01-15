{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Shader
  ( ShaderInputVertex (..)
  )
  where

import GHC.Generics (Generic)
import Linear (V2 (..), V3 (..))
import Foreign.Storable.Generic (GStorable)

import Offset

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderInputVertex
