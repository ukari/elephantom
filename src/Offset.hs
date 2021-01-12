
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Offset
  ( module Offset
  ) where


import Foreign.Storable (Storable)
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs)
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)

import Tmp
import Shader

class (Data a, Storable a) => Offset a where
  offsetof :: a -> String -> Int
--  offsetof _ = $(offsetOf' (undefined::a))

instance Offset ShaderInputVertex where
  offsetof _ field = $(offsetOf' (undefined::ShaderInputVertex)) field
