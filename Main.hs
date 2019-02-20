module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Graphics.Gnuplot.Simple

import Perceptron
import Neuron (perceptronFunction)

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

plotSin :: IO ()
plotSin = plotPath [(Title "Sin")] $ zip <$> id <*> fmap sin $ linearScale 1000 (0.0 :: Double, 2*pi)

plotPerceptron :: IO ()
plotPerceptron = plotPath [(Title "Perceptron")] $ zip <$> id <*> fmap perceptronFunction $ linearScale 1000 (-2.0 :: Double, 2.0)
