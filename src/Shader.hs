{-# LANGUAGE InstanceSigs #-}
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
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

--import Foreign.Storable.Offset.Internal.OffsetTH

import Offset -- (makeOffset)

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderInputVertex
--bar = $(stringE . show =<< makeOffset ''ShaderInputVertex)

-- bar = $(stringE . show =<< makeOffset ''ShaderInputVertex)
-- instance Offset ShaderInputVertex where
--        offsetof _ = $(offsetOf ''ShaderInputVertex)
