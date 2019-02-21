module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Graphics.Gnuplot.Simple

import Perceptron
import Neuron (perceptronFunction, sigmoidFunction, stepFunction, reluFunction)

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

v2 :: R.Array R.U R.DIM3 Double
v2 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2 R.:. 4) [1..24]

rands :: R.Array R.U R.DIM1 Double
rands = randomishDoubleArray (R.Z R.:. 10) 0.4 0.6 1

plot :: (Double -> Double) -> (Double, Double) -> IO ()
plot f (l, r) = plotPath [] $ zip <$> id <*> fmap f $ linearScale points (l, r)
  where points = round ((r - l)/4.0e-3)

plotSin :: IO ()
plotSin = plot sin (0.0, 2*pi)

plotPerceptron :: IO ()
plotPerceptron = plot perceptronFunction (-2.0, 2.0)

plotSigmoid :: IO ()
plotSigmoid = plot sigmoidFunction (-10.0, 10.0)

plotStep :: IO ()
plotStep = plot stepFunction (-2.0, 2.0)

plotReLU :: IO ()
plotReLU = plot reluFunction (-2.0, 5.0)

x1, x2 :: R.Array R.U R.DIM2 Double
-- x1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 2) [1..4]
-- x2 = R.fromListUnboxed (R.Z R.:. 2 R.:. 2) [5..8]
x1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 3) [1..6]
x2 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2) [1..6]
-- R.rank $ R.extent $ mmultS x1 x2


