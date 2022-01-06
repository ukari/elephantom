{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Shader
  ( ShaderUniform (..)
  , ShaderInputVertex (..)
  , Texture (..)
  , TriQuadVertex (..)
  , Contour (..)
  )
  where

import Linear (V2 (..), V4 (..), M44)

import Data.Int (Int16)

import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

import Offset

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V4 Float)
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
  , color :: !(V4 Float)
  , texCoord :: !(V2 Float)
  } deriving (Generic, GStorable)

makeOffset ''Texture

-- | TriQuadVertex
-- not support gradient in full color range
data TriQuadVertex = TriQuadVertex
  { position :: !(V2 Float)
  , color :: !(V4 Float)
  } deriving (Generic, GStorable)

makeOffset ''TriQuadVertex

data Contour = Contour
  { p1 :: !(V2 Int16)
  , p2 :: !(V2 Int16)
  , p3 :: !(V2 Int16)
  } deriving (Generic, GStorable, Show)

makeOffset ''Contour
