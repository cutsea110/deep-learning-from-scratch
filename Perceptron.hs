module Perceptron where

import qualified Data.Array.Repa as R
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

rands :: R.Array R.U R.DIM1 Double
rands = randomishDoubleArray (R.Z R.:. 10) 0.4 0.6 1
