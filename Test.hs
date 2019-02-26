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
plots fs rng@(l, r) = plotPaths [] $ zipWith (\f -> fmap ((,) <$> id <*> f)) fs xss
  where
    points = round ((r - l)/4.0e-3)
    xss = repeat (linearScale points rng)

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

numericalDiff f x = (f (x+h) - f (x-h)) / (2*h)
  where
    h = 1e-4

fun1 x = 0.01 * (x^2) + 0.1 * x

plotFun1 :: IO ()
plotFun1 = plot fun1 (0.0,20.0)

-- gradient at (5, 0.75(= fun1 5))
plotWithGrad1 :: IO ()
plotWithGrad1 = plots [fun1, genGrad fun1 5] (0.0, 20.0)

-- gradient at (10, 2.0(= fun1 10))
plotWithGrad2 :: IO ()
plotWithGrad2 = plots [fun1, genGrad fun1 10] (0.0, 20.0)

genGrad :: (Double -> Double) -> Double -> (Double -> Double)
genGrad f x = \a -> numDiff * a + b
  where
    numDiff = numericalDiff f x
    p = (x, f x)
    b = f x - numDiff * x
