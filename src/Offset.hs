{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Offset
  ( module Offset
  ) where

import Language.Haskell.TH
import Foreign.Storable (Storable)
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs)
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)

import Tmp
--import Shader



class (Storable a) => Offset a where
  offsetof :: a -> OffsetSelect -> Int
  --offsetof _ = $(offsetOfN (mkName "a"))

-- instance Offset ShaderInputVertex where
--   offsetof _ field = $(offsetOf' (undefined::ShaderInputVertex)) field

