{-# LANGUAGE TemplateHaskell #-}

module Tmp
  ( foo
  ) where

import Language.Haskell.TH

foo :: Q Exp
foo = do
  let x = mkName "x"
  lamE [varP x] (varE x)

-- offsetOf :: Q Exp
-- offsetOf
