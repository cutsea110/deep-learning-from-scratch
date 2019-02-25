module Test where

import Graphics.Gnuplot.Simple
import Activation ( perceptron
                  , sigmoid
                  , step
                  , relu
                  )
plot :: (Double -> Double) -> (Double, Double) -> IO ()
plot f (l, r) = plotPath [] $ zip <$> id <*> fmap f $ linearScale points (l, r)
  where points = round ((r - l)/4.0e-3)

plotSin :: IO ()
plotSin = plot sin (0.0, 2*pi)

plotPerceptron :: IO ()
plotPerceptron = plot perceptron (-2.0, 2.0)

plotSigmoid :: IO ()
plotSigmoid = plot sigmoid (-10.0, 10.0)

plotStep :: IO ()
plotStep = plot step (-2.0, 2.0)

plotReLU :: IO ()
plotReLU = plot relu (-2.0, 5.0)

numericalDiff f x = (f (x+h) - f (x-h)) / 2*h
  where
    h = 1e-4
