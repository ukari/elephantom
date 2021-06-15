{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Conv
  where


import Data.Time (getCurrentTime, diffUTCTime)
import Data.Vector.Unboxed (iterateN, generate, unfoldrExactN)
import qualified Data.Vector.Unboxed as VU
import Data.Massiv.Array hiding ((!*!), R, iterateN, generate, toList, fromList)
import Data.Massiv.Array.Manifest.Vector (VRepr, ARepr, toVector, fromVector')


import Numeric.AD
import System.Random

import Data.Bifunctor (first)
import Control.Applicative (liftA2)
import Control.Scheduler (getCompWorkers)

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

testsh :: IO ()
testsh = do
  print =<< getCompWorkers Par
  print =<< getCompWorkers Par'
  

testdot :: IO ()
testdot = do
  let rd = mkStdGen 2
  let a0 = randomArray rd split random Par (Sz 10000000) :: Array DL (Ix 1) Double
  let !a1 = computeAs U a0 :: Array U Ix1 Double
  count $ print $ a1 !.! a1
  --count $ print $ normL2 a1

testdot2 :: IO ()
testdot2 = do
  let !v = (fromUnboxedVector Par $ unfoldrExactN 10000000 random (mkStdGen 2)) :: Array U (Ix 1) Double
  count $ print $ v !.! v

count :: IO () -> IO ()
count f = do
  tic <- getCurrentTime
  f
  toc <- getCurrentTime
  print $ diffUTCTime toc tic

