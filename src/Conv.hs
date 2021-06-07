{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Conv () where

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.IO.Matrix
import Numeric.AD

-- https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial

test :: IO ()
test = do
  let array = fromList (Z:.3:.2::DIM2) [0..5::Int] :: Array U DIM2 Int
  --writeMatrixToTextFile "/tmp/nil/test.txt" array
  
  pure ()

foo = fromListUnboxed (Z:.3:.2:.4) [1..24] :: Array U DIM3 Int
