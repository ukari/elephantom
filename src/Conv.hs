{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Conv
  ( tests
  ) where

import Data.Time (getCurrentTime, diffUTCTime)
import Data.Vector.Unboxed (iterateN, generate, unfoldrExactN)
import qualified Data.Vector.Unboxed as VU
import Data.Massiv.Array hiding ((!*!), R, iterateN, generate, toList, fromList)
import Data.Massiv.Array.Manifest.Vector (VRepr, ARepr, toVector, fromVector')
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

testdot :: Array U (Ix 1) Double -> IO ()
testdot a1 = do
  --let v0 = toVector a1 :: VU.Vector Double
  tic <- getCurrentTime
  print $ a1 !.! a1
  toc <- getCurrentTime
  print $ diffUTCTime toc tic

  pure ()

testdot2 :: Array U (Ix 1) Double -> IO ()
testdot2 !a0 = do
  
  --let v0 = toVector a1 :: VU.Vector Double
  tic <- getCurrentTime
  let !v = a0 !.! a0
  toc <- getCurrentTime
  print v
  print $ diffUTCTime toc tic
  pure ()

testh :: R 10000000 -> IO ()
testh v = do
  -- let a = row (vec4 1 2 3 4)
  --         ===
  --         row (vec4 5 6 7 8)
  -- print $ tr a
  -- let rd = mkStdGen 2
  -- let rdv = unfoldrExactN @Float 10 random rd
  
  tic <- getCurrentTime
  print $ v `dot` v
  toc <- getCurrentTime

  print $ diffUTCTime toc tic
  pure ()

testh2 :: R 10000000 -> IO ()
testh2 !v = do
  -- let a = row (vec4 1 2 3 4)
  --         ===
  --         row (vec4 5 6 7 8)
  -- print $ tr a
  -- let rd = mkStdGen 2
  -- let rdv = unfoldrExactN @Float 10 random rd
  
  tic <- getCurrentTime
  let !b = v `dot` v
  toc <- getCurrentTime
  print b
  print $ diffUTCTime toc tic
  pure ()

tests :: IO ()
tests = do

  tic0 <- getCurrentTime
  let rd = mkStdGen 2
  -- change Comp strategy will cause the double acc result changed
  -- Seq 3333366.1108324886 vs Par 3333366.1108324807
  let a0 = randomArray rd split random Par (Sz 10000000) :: Array DL (Ix 1) Double
  --print a0
  let !a1 = computeP a0 :: Array U Ix1 Double
  toc0 <- getCurrentTime
  print $ "testdot load: " <> show (diffUTCTime toc0 tic0)
  print "testdot:"
  testdot a1

  tic0 <- getCurrentTime
  let rd = mkStdGen 2
  let !a0 = fromUnboxedVector Par (unfoldrExactN 10000000 random (mkStdGen 2)) :: Array U (Ix 1) Double
  --print a0
  --let a1 = computeP a0 :: Array U Ix1 Double
  toc0 <- getCurrentTime
  print $ "testdot2 load: " <> show (diffUTCTime toc0 tic0)
  print "testdot:"
  testdot a0
  print "testdot2:"
  testdot2 a0
  


  tic0 <- getCurrentTime
  --let v = vector $ VU.toList rdv :: R 10
  let !v = vector (VU.toList $ unfoldrExactN 10000000 random (mkStdGen 2) :: [Double]) :: R 10000000
  toc0 <- getCurrentTime
  print $ "testh load: " <> show (diffUTCTime toc0 tic0)
  print "testh:"
  testh v
  print "testh2:"
  testh2 v
  
  pure ()
-- summary by test dot
-- massiv is better in
-- - deploy
-- - memory usage (10x smaller
-- - [compile (massiv ParN 16)] calculate speed (2.17x faster, 0.003852603s vs 0.008387589s
-- - [compile (massiv Seq)] load speed (17.9x faster, 0.705162433s vs 12.532785942s
-- - [compile (massiv ParN 16)] load speed (15x faster, 0.824644608s vs 15.217289053398058s
-- hmatrix is better in
-- - [interp] calculate speed (137x faster, 0.007943513s vs 1.092581737s
-- - [compile (massiv Seq)] calculate speed (1.25x faster, 0.008230806s vs 0.010627114s
