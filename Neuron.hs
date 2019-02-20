module Neuron where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish

-- h is activation function
generate (w1, w2, bias, h) = neuron
  where
    neuron x1 x2 = h tmp
      where
        x, w :: R.Array R.U R.DIM2 Double
        x = R.fromListUnboxed (R.Z R.:. 2 R.:. 1) [x1, x2]
        w = R.fromListUnboxed (R.Z R.:. 2 R.:. 1) [w1, w2]
        -- tmp = b + R.sumAllS (mmultS x w)
        tmp = runST $ do
          t <- mmultP x w
          s <- R.sumAllP t
          return $ bias + s

perceptronFunction x | x <= 0    = 0
                     | otherwise = 1

stepFunction x | x > 0     = 1
               | otherwise = 0

sigmoidFunction x = 1 / (1 + exp (-x))

reluFunction = max 0.0
