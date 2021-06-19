{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
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

import qualified Data.ByteString.Lazy as BL
--import Data.ByteString.Builder
import Data.Binary.Get (Get, runGet, getWord32be)
import Data.Attoparsec.ByteString.Lazy (Parser, Result (..), anyWord8, count, word8, parse, takeLazyByteString)
import qualified Data.Attoparsec.ByteString.Lazy as Attoparsec
import Numeric.AD
import System.Random
import Conduit
import Data.Conduit.Attoparsec (conduitParserEither)

import Data.Word (Word8, Word16, Word32)
import Data.Functor (void)
import Data.Bifunctor (first)
import Control.Applicative (liftA2)
import Control.Scheduler (getCompWorkers)

wx :: IO ()
wx = do
  let w = fromLists' Seq [[0,0.1,0.2,0.3], [0.4,0.5,0.6,0.7]] :: Array U Ix2 Double
  let x = fromLists' Seq [[0, 1,2,3,4], [5,6,7,8,9], [0,1,2,3,4], [5,6,7,8,9]] :: Array U Ix2 Double
  print w
  print x
  print $ w !><! x
  pure ()

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
  tcount $ print $ a1 !.! a1
  --count $ print $ normL2 a1

testdot2 :: IO ()
testdot2 = do
  let !v = (fromUnboxedVector Par $ unfoldrExactN 10000000 random (mkStdGen 2)) :: Array U (Ix 1) Double
  tcount $ print $ v !.! v

tcount :: IO () -> IO ()
tcount f = do
  tic <- getCurrentTime
  f
  toc <- getCurrentTime
  print $ diffUTCTime toc tic

-- x1 = [1,2]
-- x2 = [100,200]
-- u = [50.5,101]
-- x1' = [-49.5, -99]
-- x2' = [49.5, 99]

--
--
--
-- (x1^2 - x2^2)/(x1^2 + x2^2)
--
-- w1 * x1 + b
-- w2 * x2 + b
-- w3 * x3 + b
-- ...
-- wm * xm + b

-- W*X

-- Wl = 0.5 /10
       -- 0.6
       -- 0.1
       -- 0.3
-- X = [2,3,4,5]
-- W*X = 1    1.5  2  2.5    1.5^0.5/4^0.5   y = 2  al = (E(yhat i - y)^2)^0.5
         -- 1.2  1.8
         -- 0.2
         -- 0.6



-- theta (WX) =

-- X -> W1X = A1 -> W2*A1 = A2

-- [m特征值 x n样本数]  [mx1]



-- 样本数 m
-- 特征数 n
-- x n维向量
-- X n行xm列
-- Y 1行xm列
-- w n维向量
-- w^t w的转置
-- b 实数
-- z 设z为w^t * x + b
-- yhat 单样本预测值 yhat = sigma(w^t * x + b) = sigma(z)
-- sigma(z) = 1 / (1 + e ^ (-z))
-- loss(yhat, y) = - (y * log(yhat) + (1 - y) * log(1 - yhat)) 交叉熵损失函数 Cross Entropy Loss
-- J(w,b) = (1/m) * sum(loss(yhat(i), y(i))) 成本函数 cost function
-- https://towardsdatascience.com/using-the-right-dimensions-for-your-neural-network-2d864824d0df

data Input = Input Int deriving (Show)

data Layer = FullyConnected (Array U Ix2 Double) Double deriving (Show)

mkWeight :: StdGen -> Int -> Int -> Array U Ix2 Double
mkWeight rseed featureNum sampleNum = computeAs U . resize' (Sz (featureNum :. sampleNum)) . randomArray rseed split random Par . Sz $ featureNum * sampleNum

mkFull :: StdGen -> Int -> Int -> Layer
mkFull rseed featureNum sampleNum = FullyConnected w b where
  b :: Double
  nrseed :: StdGen
  (b, nrseed) = random rseed
  w :: Array U Ix2 Double
  w = computeAs U . resize' (Sz (featureNum :. sampleNum)) . randomArray nrseed split random Par . Sz $ featureNum * sampleNum

testimnn :: IO ()
testimnn = do
  --let input = Input 2
  let layer1 = mkFull (mkStdGen 0) 3 2
  let layer2 = mkFull (mkStdGen 1) 2 3
  let layer3 = mkFull (mkStdGen 2) 1 2
  print layer1
  print layer2
  print layer3
  pure ()


-- mnist idx
data Header = Header
  { dtype :: !Word8
  , dims :: ![ Word32 ]
  }
  deriving (Show)

dim :: Parser Word32
dim = do
  bs <- Attoparsec.take 4
  pure . runGet getWord32be . BL.fromStrict $ bs

idx :: Parser (Header, BL.ByteString)
idx = do
  void . count 2 . word8 $ 0
  dtype <- anyWord8
  dimNum <- anyWord8
  dims <- count (fromIntegral dimNum) dim
  datas <- takeLazyByteString
  pure (Header { dtype, dims }, datas)

load :: IO ()
load = do
  trainImagesBL <- BL.readFile "/tmp/nil/mnist/train-images.idx3-ubyte"
  case parse idx trainImagesBL of
    Done partial (h, res) -> print h
    _ -> error "fail to load"
  pure ()
