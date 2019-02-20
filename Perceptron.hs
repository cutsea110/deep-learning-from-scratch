module Perceptron where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish

pAND x1 x2 | tmp <= theta = 0
           | otherwise    = 1
  where
    (w1, w2, theta) = (0.5, 0.5, 0.7)
    tmp = x1 * w1 + x2 * w2

pNAND x1 x2 | tmp <= theta = 0
            | otherwise    = 1
  where
    (w1, w2, theta) = (-0.5, -0.5, -0.7)
    tmp = x1 * w1 + x2 * w2

pOR x1 x2 | tmp <= theta = 0
          | otherwise    = 1
  where
    (w1, w2, theta) = (0.5, 0.5, 0.2)
    tmp = x1 * w1 + x2 * w2

x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.Z R.:. 3 R.:. 2) [0..5]

main :: IO ()
main = do
  putStrLn "This is sample of Repa."
  print rands
  print $ mmultS y v1
  print $ runST $ mmultP y v1

y :: R.Array R.U R.DIM2 Double
y = R.fromListUnboxed (R.Z R.:. 2 R.:. 2) [1..4]

v1 :: R.Array R.U R.DIM2 Double
v1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 1) [5,6]

rands :: R.Array R.U R.DIM1 Double
rands = randomishDoubleArray (R.Z R.:. 10) 0.4 0.6 1
