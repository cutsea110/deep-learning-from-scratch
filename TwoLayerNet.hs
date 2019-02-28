{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module TwoLayerNet where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import qualified Data.Array.Repa as R
import Data.Vector.Unboxed.Base
import Data.Word
import System.Random

import Activation (sigmoid, softmaxS)
import Neuron (forwardS, forwardP)
import Mnist (DataSet, Matrix, Vector, imageAt, labelAt)
import Util

import Mnist (downloadMnist, loadTrain, loadTest, draw)

initLayer :: (Int, Int) -> (Double, Double) -> IO (R.Array R.U R.DIM2 Double)
initLayer (iSz, hSz) (l, u) = do
  inits <- replicateM (iSz*hSz) $ getStdRandom $ randomR (l, u)
  return $ R.fromListUnboxed (R.Z R.:. iSz R.:. hSz) inits

initBias :: Int -> Double -> R.Array R.U R.DIM1 Double
initBias hSz = R.fromListUnboxed (R.Z R.:. hSz) . take hSz . repeat

initTwoLayerNet iSz hSz oSz (l, u) b = do
  w1 <- initLayer (iSz, hSz) (l, u)
  w2 <- initLayer (hSz, oSz) (l, u)
  let b1 = initBias hSz b
      b2 = initBias oSz b
  return [ (sigmoid, w1, b1)
         , (id, w2, b2)
         ]

predict x net = softmaxS $ forwardS x net

randomSampling n (imgs, lbls) = do
  idxs <- replicateM n $ getStdRandom $ randomR (0, imgrsz)
  let is = R.backpermute (R.Z R.:. n R.:. imgcsz) (transSh idxs) imgs
      ls = R.backpermute (R.Z R.:. n R.:. lblcsz) (transSh idxs) lbls
  return $ (R.computeUnboxedS is, R.computeUnboxedS ls)
    where
      (imgrsz, imgcsz) = rowCount &&& colCount $ imgs
      (lblrsz, lblcsz) = rowCount &&& colCount $ lbls
      transSh choices ix = let (c:r:[]) = R.listOfShape ix in R.Z R.:. (choices !! r) R.:. c
  
main = do
  (xi, xl) <- loadTrain
  -- normalize
--  let xi = R.map ((/w).fromIntegral) _xi
  let (r, c) = (rowCount xi, colCount xi)
  net <- initTwoLayerNet c hiddenSize outputSize (0.0, 1.0) 0.0
  (sImgs, sLbls) <- randomSampling batchSize (xi, xl)
  
  putStrLn "Done."
  
  where
    hiddenSize = 100
    outputSize = 10
    batchSize = 100
    w :: Double
    w = fromIntegral (maxBound - minBound :: Word8) 
