{-# LANGUAGE FlexibleContexts, TypeOperators, ScopedTypeVariables, AllowAmbiguousTypes,BangPatterns #-}
module TwoLayerNet where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix (row, col)
import Data.Time
import Data.Vector.Unboxed.Base
import Data.Word
import System.Random

import Activation (sigmoid, softmaxS, softmaxP)
import Loss (ceeS, ceeP)
import Mnist (DataSet, Matrix, Vector, imageAt, labelAt)
import Neuron (forwardS, forwardP)
import Util

import Mnist (loadTrain, loadTest, draw)

initLayer :: (Int, Int) -> (Double, Double) -> IO (R.Array R.U R.DIM2 Double)
initLayer (iSz, hSz) (l, u) = do
  inits <- replicateM (iSz*hSz) $ getStdRandom $ randomR (l, u)
  return $ R.fromListUnboxed (R.ix2 iSz hSz) inits

initBias :: Int -> Double -> R.Array R.U R.DIM1 Double
initBias hSz = R.fromListUnboxed (R.ix1 hSz) . take hSz . repeat

initTwoLayerNet iSz hSz oSz (l, u) b = do
  w1 <- initLayer (iSz, hSz) (l, u)
  w2 <- initLayer (hSz, oSz) (l, u)
  let b1 = initBias hSz b
      b2 = initBias oSz b
  return [ (sigmoid, w1, b1)
         , (id, w2, b2)
         ]

predict net x = softmaxS $ forwardS x net

predictP net x = do
  y <- forwardP x net
  softmaxP y

loss net x t = ceeS (predict net x) t

lossP net x t = do
  z <- predictP net x
  ceeP z t

numGrad net@((a1,w1,b1):(a2,w2,b2):[]) x t = [(gradw1,gradb1),(gradw2,gradb2)]
  where
    fw1 w1' = loss [(a1,R.computeUnboxedS w1',b1),(a2,w2,b2)] x t
    fb1 b1' = loss [(a1,w1,R.computeUnboxedS b1'),(a2,w2,b2)] x t
    fw2 w2' = loss [(a1,w1,b1),(a2,R.computeUnboxedS w2',b2)] x t
    fb2 b2' = loss [(a1,w1,b1),(a2,w2,R.computeUnboxedS b2')] x t

    gradw1 = numericalGradient fw1 w1
    gradb1 = numericalGradient fb1 b1
    gradw2 = numericalGradient fw2 w2
    gradb2 = numericalGradient fb2 b2

randomSampling n (imgs, lbls) = do
  idxs <- replicateM n $ getStdRandom $ randomR (0, imgrsz)
  let dict = R.fromListUnboxed (R.ix1 n) idxs
  let is = R.backpermute (R.ix2 n imgcsz) (transSh dict) imgs
      ls = R.backpermute (R.ix2 n lblcsz) (transSh dict) lbls
  return $ (R.computeUnboxedS is, R.computeUnboxedS ls)
    where
      (imgrsz, imgcsz) = row &&& col $ R.extent imgs
      (lblrsz, lblcsz) = row &&& col $ R.extent lbls
      transSh vec (R.Z R.:. r R.:. c) = R.ix2 (vec R.! (R.ix1 r)) c

main = do
  let (hiddenSize, outputSize, batchSize, w) = (50, 10, 100, fromIntegral (maxBound :: Word8)::Double)

  (_xi, _xl) <- loadTrain
  -- normalize
  let (xi :: R.Array R.D R.DIM2 Double, xl :: R.Array R.D R.DIM2 Double)
        = (R.map ((/w).fromIntegral) _xi, R.map fromIntegral _xl)
  let (shxi, r, c) = (R.extent xi, row shxi, col shxi)
  net <- initTwoLayerNet c hiddenSize outputSize (0.0, 1.0) 0.0
  (!sImgs, !sLbls) <- randomSampling batchSize (xi, xl)

  -- test
  let ret@((gw1, gb1):(gw2, gb2):[]) = numGrad net sImgs sLbls
  print "gb2"
  print $ R.computeUnboxedS gb2
  print "gb1"
  print $ R.computeUnboxedS gb1
  print "gw2"
  print $ R.computeUnboxedS gw2
  print "gw1"
  print $ R.computeUnboxedS gw1

  putStrLn "Done."
