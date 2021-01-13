{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Offset
  ( module Offset
  ) where

import Language.Haskell.TH
import Foreign.Storable (Storable)
import Data.Data (Data, Typeable, constrFields, toConstr, dataTypeConstrs, dataTypeOf, maxConstrIndex, indexConstr, typeRepArgs)
import Foreign.Storable.Generic (GStorable, gsizeOf, galignment, peek)

import Tmp
--import Shader

class Offset a where
  offsetof :: a -> OffsetSelect -> Int

makeOffset :: Name -> Q [Dec]
makeOffset iname = do
  let func = offsetOfN iname :: Q Exp
  [d|instance Offset iname where
       offsetof _ = $(func)
    |]
