module Test where

import Activation ( perceptron
                  , sigmoid
                  , step
                  , relu
                  )
import Util

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

fun1 x = 0.01 * (x^2) + 0.1 * x

plotFun1 :: IO ()
plotFun1 = plot fun1 (0.0,20.0)

-- gradient at (5, 0.75(= fun1 5))
plotWithGrad1 :: IO ()
plotWithGrad1 = plots [fun1, genGrad fun1 5] (0.0, 20.0)

-- gradient at (10, 2.0(= fun1 10))
plotWithGrad2 :: IO ()
plotWithGrad2 = plots [fun1, genGrad fun1 10] (0.0, 20.0)
