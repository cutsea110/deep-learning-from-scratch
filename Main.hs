module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Graphics.Gnuplot.Simple

import Perceptron
import Neuron (perceptronFunction, sigmoidFunction, stepFunction, reluFunction)

main :: IO ()
main = do
  putStrLn "This is sample of Repa."

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

x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.Z R.:. 1 R.:. 2) [1.0, 0.5]
w1 :: R.Array R.U R.DIM2 Double
w1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
b1 :: R.Array R.U R.DIM2 Double
b1 = R.fromListUnboxed (R.Z R.:. 1 R.:. 3) [0.1, 0.2, 0.3]

w2 :: R.Array R.U R.DIM2 Double
w2 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
b2 :: R.Array R.U R.DIM2 Double
b2 = R.fromListUnboxed (R.Z R.:. 1 R.:. 2) [0.1, 0.2]

w3 :: R.Array R.U R.DIM2 Double
w3 = R.fromListUnboxed (R.Z R.:. 2 R.:. 2) [0.1, 0.3, 0.2, 0.4]
b3 :: R.Array R.U R.DIM2 Double
b3 = R.fromListUnboxed (R.Z R.:. 1 R.:. 2) [0.1, 0.2]


a1 = triple id x w1 b1
z1 = triple sigmoidFunction x w1 b1
a2 = triple id z1 w2 b2
z2 = triple sigmoidFunction z1 w2 b2
a3 = triple id z2 w3 b3
y = triple id z2 w3 b3

triple f x w b = runST $ do
  xw <- mmultP x w
  R.computeUnboxedP $ R.zipWith ((f.).(+)) xw b1
