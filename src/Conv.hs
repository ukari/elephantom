{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}


module Conv
  (
  ) where

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Vector.Unboxed (iterateN, generate, unfoldrExactN)
import qualified Data.Vector.Unboxed as VU
import Data.Massiv.Array hiding ((!*!), R, iterateN, generate, toList, fromList)
import Data.Massiv.Array.Manifest.Vector (VRepr, ARepr, toVector)
import Numeric.LinearAlgebra.Data (reshape)
import Numeric.LinearAlgebra.Static (R (..), Domain(..), Sized (..), (===), vec4, row, tr, vector)

--import Data.Massiv.Array.Numeric ((!*!))
--import Linear.V
--import Linear
--import Linear.Matrix
import Control.Lens.Setter (ASetter')
import Numeric.AD
import System.Random


import qualified Data.Vector.Generic as VG
import Data.Bifunctor (first)
import Control.Applicative (liftA2)



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

test2 :: IO ()
test2 = do
  let a0 = 1...64 :: Array D Ix1 Int
  a2 <- resizeM (Sz (8 :. 8)) a0
  let a3 = compute a2 :: Array U Ix2 Int
  print $ getComp a3
  let st = makeStencil (Sz (3 :. 3)) (1 :. 0) (\get -> get (0 :. 2))
  print a0
  print $ computeAs U $ (+0) <$> mapStencil Wrap st a3
  pure ()

testdot :: IO ()
testdot = do
  tic0 <- getCurrentTime
  let rd = mkStdGen 2
  let a0 = randomArray rd split (random) (ParN 16) (Sz 10000000) :: Array DL (Ix 1) Double
  --print a0
  let a1 = computeP a0 :: Array U Ix1 Double
  toc0 <- getCurrentTime
  print $ diffUTCTime toc0 tic0
  --let v0 = toVector a1 :: VU.Vector Double
  tic <- getCurrentTime
  let v = a1 !.! a1
  toc <- getCurrentTime
  print v
  print $ diffUTCTime toc tic

  pure ()

testh ::IO ()
testh = do
  let a = row (vec4 1 2 3 4)
          ===
          row (vec4 5 6 7 8)
  print $ tr a
  let rd = mkStdGen 2
  let rdv = unfoldrExactN @Float 10 random rd
  tic0 <- getCurrentTime
  --let v = vector $ VU.toList rdv :: R 10
  let v = vector (VU.toList $ unfoldrExactN 10000000 random (mkStdGen 2) :: [Double]) :: R 10000000
  toc0 <- getCurrentTime
  print $ diffUTCTime toc0 tic0
  tic <- getCurrentTime
  let b = v `dot` v
  toc <- getCurrentTime
  print b
  print $ diffUTCTime toc tic


  pure ()

foob = random @Float $ snd $ (random @Float) $ mkStdGen 2
