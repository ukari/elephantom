{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conv () where

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.IO.Matrix
import Data.Array.Repa.Algorithms.Matrix (mmultP, mmultS)
import Numeric.AD

-- https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial
-- martix Z:.2:.4 means 2col 4row
-- fromList [0,1,2,3,4,5,6,7]
-- | 0 1 2 3 |
-- | 4 5 6 7 |

test :: IO ()
test = do
  let array = fromList (Z:.6:.6:.1) [0..35::Int] :: Array U DIM3 Int
  let martix0 = fromListUnboxed (Z:.2:.4::DIM2) [0..7::Double]
  print martix0
  (mt :: Array U DIM2 Double) <- computeUnboxedP (transpose martix0 :: Array D (DIM2) Double)
  print mt
  print $ listOfShape $ extent martix0
  print $ listOfShape $ extent mt
  let y = fromListUnboxed (Z:.3:.3::DIM2) [1..9::Double]
  result <- mt `mmultP` y
  print result
  pure ()

foo = fromListUnboxed (Z:.3:.2:.4) [1..24] :: Array U DIM3 Int
