{-# LANGUAGE OverloadedStrings #-}

module GLSL
  (-- vert
  --, frag
  ) where

import SpirV
--import qualified Vulkan.Utils.ShaderQQ as Vulkan
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

vert :: QuasiQuoter
vert = QuasiQuoter
  { quoteExp = quoteExpShader Vert
  , quotePat = error "not support"
  , quoteType = error "not support"
  , quoteDec = error "not support"
  }

quoteExpShader :: ShaderStage -> String -> Q Exp
quoteExpShader = undefined
