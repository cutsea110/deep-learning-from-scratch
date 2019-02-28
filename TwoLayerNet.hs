{-# LANGUAGE TypeOperators #-}
module TwoLayerNet where

import Control.Monad (replicateM)
import qualified Data.Array.Repa as R
import Data.Vector.Unboxed.Base
import System.Random

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
  return (w1, b1, w2, b2)


main = do
  (xl, xi) <- loadTrain
  let (r, c) = (rowCount xi, colCount xi)
  net <- initTwoLayerNet c 100 10 (0.0, 1.0) 0.0
  
  putStrLn "Done."
  
  where
    batchSize = 100
