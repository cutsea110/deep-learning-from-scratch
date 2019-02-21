module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.List (foldl')
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish
import Graphics.Gnuplot.Simple

import Perceptron
import Neuron ( perceptronFunction
              , sigmoidFunction
              , stepFunction
              , reluFunction
              , forward
              , triple
              )

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

{-
a1 = triple x (id, w1, b1)
z1 = triple x (sigmoidFunction, w1, b1)
a2 = triple z1 (id, w2, b2)
z2 = triple z1 (sigmoidFunction, w2, b2)
a3 = triple z2 (id, w3, b3)
y = triple z2 (id, w3, b3)
-}


x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.Z R.:. 1 R.:. 2) [1.0, 0.5]

initNetwork = [ (sigmoidFunction, w1, b1)
              , (sigmoidFunction, w2, b2)
              , (id, w3, b3)
              ]
  where
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

test = forward x initNetwork
