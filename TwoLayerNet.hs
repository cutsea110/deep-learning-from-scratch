module TwoLayerNet where

import Control.Monad (replicateM)
import qualified Data.Array.Repa as R
import System.Random

import Util

import Mnist (downloadMnist, loadTrain, loadTest, draw)



main = do
  (xl, xi) <- loadTrain
  let n = rowCount xl
      g = randomR (0, n)
  idxs <- replicateM batchSize $ getStdRandom g
  
  putStrLn "Done."
  
  where
    batchSize = 100
