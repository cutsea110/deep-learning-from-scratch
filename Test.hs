module Test where

import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import Activation ( perceptron
                  , sigmoid
                  , step
                  , relu
                  )
plot :: (Double -> Double) -> (Double, Double) -> IO ()
plot f = plots [f]

plots :: (RealFrac a, Tuple.C a) => [a -> a] -> (a, a) -> IO ()
plots fs (l, r) = plotPaths [] $ zipWith (\f xs -> fmap (\x -> (x, f x)) xs) fs (repeat (linearScale points (l, r)))
  where points = round ((r - 1)/4.0e-3)

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

fun1 x = 0.01*x^2 + 0.1*x
x = [0.0,0.1..20.0]
plotFun1 :: IO ()
plotFun1 = plot fun1 (0.0,20.0)
