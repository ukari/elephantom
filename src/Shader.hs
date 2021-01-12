{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Shader
  (module Shader)
  where

--import Tmp

import Linear (V2 (..), V3 (..))
import GHC.Generics ((:*:) (..), Generic (..), M1 (..), K1 (..))
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs)
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)
import Foreign.Storable (Storable (sizeOf))

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable, Data)

-- instance Offset ShaderInputVertex where
--   offsetOf x field = $(offsetOf' undefined::ShaderInputVertex) field
