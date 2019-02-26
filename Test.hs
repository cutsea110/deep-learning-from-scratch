 module Test where

import qualified Data.Array.Repa as R
 
import Activation ( perceptron
                  , sigmoid
                  , step
                  , relu
                  )
import Util
import Graphics.Gnuplot.Simple

plotSin :: IO ()
plotSin = plot sin (0.0 :: Double, 2*pi)

plotPerceptron :: IO ()
plotPerceptron = plot perceptron (-2.0 :: Double, 2.0)

plotSigmoid :: IO ()
plotSigmoid = plot sigmoid (-10.0 :: Double, 10.0)

plotStep :: IO ()
plotStep = plot step (-2.0 :: Double, 2.0)

plotReLU :: IO ()
plotReLU = plot relu (-2.0 :: Double, 5.0)

fun1 :: Double -> Double
fun1 x = 0.01 * (x^2) + 0.1 * x

plotFun1 :: IO ()
plotFun1 = plot fun1 (0.0, 20.0)

-- gradient at (5, 0.75(= fun1 5))
plotWithGrad1 :: IO ()
plotWithGrad1 = plots [fun1, genGrad fun1 5] (0.0, 20.0)

-- gradient at (10, 2.0(= fun1 10))
plotWithGrad2 :: IO ()
plotWithGrad2 = plots [fun1, genGrad fun1 10] (0.0, 20.0)

plotWithGrad3 :: IO ()
plotWithGrad3 = plots [fun1, genGrad fun1 1, genGrad fun1 5, genGrad fun1 10] (0.0, 20.0)

numGrad1 = R.computeUnboxedS $ numericalGradient f (R.fromListUnboxed (R.Z R.:.2) [3.0, 4.0] :: R.Array R.U R.DIM1 Double)
  where
    f x = R.sumAllS $ R.map (^2) x

plotGrad = plotPathStyle [] style $ zip grids vecs'
  where
    style = defaultStyle { plotType = Vectors, lineSpec = CustomStyle [LineType 1, LineWidth 0.5] }
    
    f x = R.sumAllS $ R.map (^2) x
    grids = [(x,y) | x <- [-2.0,-1.75..2.0], y <- [-2.0,-1.75..2.0]]
    
    args = map (\(x,y) -> R.fromListUnboxed (R.Z R.:.2) [x,y] :: R.Array R.U R.DIM1 Double) grids
    vecs = map (\x -> let r = R.computeUnboxedS (numericalGradient f x) in (r R.! (R.Z R.:. 0), r R.! (R.Z R.:.1))) args
    vecs' = map (\(v1, v2) -> (-0.05 * v1, -0.05 * v2)) vecs
    
