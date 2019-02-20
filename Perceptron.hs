module Perceptron where

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

genSteperon (w1, w2, bias) = generate (w1, w2, bias, stepFunction)

genPerceptron (w1, w2, bias) = generate (w1, w2, bias, perceptronFunction)

pAND  = genPerceptron ( 0.5,  0.5, -0.7)
pNAND = genPerceptron (-0.5, -0.5,  0.7)
pOR   = genPerceptron ( 0.5,  0.5, -0.2)
pXOR x1 x2 = pAND (pNAND x1 x2) (pOR x1 x2)

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
