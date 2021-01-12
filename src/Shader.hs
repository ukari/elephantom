{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Shader
  ( module Shader
  )
  where

import Language.Haskell.TH

import Linear (V2 (..), V3 (..))
import GHC.Generics ((:*:) (..), Generic (..), M1 (..), K1 (..))
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs)
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)
import Foreign.Storable (Storable (sizeOf))

import Tmp
import Offset

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable, Data)

-- instance Offset ShaderInputVertex where
--   offsetof _ field = $(offsetOf' (undefined::ShaderInputVertex)) field

--baz = $(stringE . show =<< reify (mkName "ShaderInputVertex"))

--data Foo = Foo Int deriving (Show)
x1 = 100

-- introducing a top-level splice, [link](https://gitlab.haskell.org/ghc/ghc/-/issues/9813)
$( return [] )

foo = $(stringE . show =<< reify (mkName "x1"))

far = [|inPosition|]

instance Offset ShaderInputVertex where
  --offsetof :: ShaderInputVertex -> OffsetSelect -> Int
  -- offsetof _ r@(Record _) = $(offsetOfN (mkName "ShaderInputVertex")) r
  -- offsetof _ i@(Normal _)= $(offsetOfN (mkName "ShaderInputVertex")) i
  offsetof _ = $(offsetOfN (mkName "ShaderInputVertex"))

bar = $(stringE . show =<< reify (mkName "Bar"))

baz = $(stringE . show =<< reify (mkName "Foo"))
baza = $(offsetOfN (mkName "Bar"))

buz = $(stringE . show =<< reify (mkName "ShaderInputVertex"))
buza = $(offsetOfN (mkName "ShaderInputVertex"))
