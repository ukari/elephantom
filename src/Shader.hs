{-# LANGUAGE FlexibleInstances #-}
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

pure []

-- fuz = ''ShaderInputVertex
-- fuz1 = 'ShaderInputVertex


-- https://downloads.haskell.org/~ghc/7.0.2/docs/html/users_guide/template-haskell.html
-- > A name can be quoted with either one or two prefix single quotes:
-- >   'f has type Name, and names the function f. Similarly 'C has type Name and names the data constructor C. In general 'thing interprets thing in an expression context.
-- >   ''T has type Name, and names the type constructor T. That is, ''thing interprets thing in a type context.
-- makeOffset 'ShaderInputVertex

-- pure []
-- instance Offset ShaderInputVertex where
--   offsetof _ = $(offsetOfN ''ShaderInputVertex)
makeOffset (mkName "ShaderInputVertex")

foo = $(stringE . show =<< makeOffset (mkName "ShaderInputVertex"))
-- bar = $(stringE . show =<< reify (mkName "Bar"))

-- baz = $(stringE . show =<< reify (mkName "Foo"))
-- baza = $(offsetOfN (mkName "Bar"))

-- buz = $(stringE . show =<< reify (mkName "ShaderInputVertex"))
-- buza = $(offsetOfN (mkName "ShaderInputVertex"))
