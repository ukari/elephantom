{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Conv
  (
  ) where

import Data.Massiv.Array hiding ((!*!))
--import Data.Massiv.Array.Numeric ((!*!))
import Linear.V
import Linear
import Linear.Matrix
import Control.Lens.Setter (ASetter')
import Numeric.AD

test :: IO ()
test = do
  let mt = singleton 1 :: Array U Ix1 Int
  let mt0 = fromLists' Par [[0..3], [4..7]] :: Array U Ix2 Int
  let mt1 = fromLists' Par [[0,1], [2,3], [4,5],[6,7]] :: Array U Ix2 Int
  print mt0
  print mt1
  print $ mt0 !><! mt1
  let lm0 = fromLists' Par [[0 .. 9999], [9999 .. 0]] :: Array U Ix2 Int
  pure $ mt1 !><! lm0

  pure ()

--foo = [0..5] :: V 6 Int
lin :: IO ()
lin = do
  let l0 = [[0 .. 9999], [9999 .. 0::Int]] :: [[Int]]
  let l2 = [[0 .. 9999::Int]] :: [[Int]]
  let foo = [[1..3], [1..3], [1..3]] :: [[Int]]
  print $ l2 !*! foo
  pure ()

