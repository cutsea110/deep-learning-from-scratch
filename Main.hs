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

a = triple id
z1 = triple sigmoidFunction

triple f = runST $ do
  xw <- mmultP x w1
  R.computeUnboxedP $ R.zipWith ((f.).(+)) xw b1

