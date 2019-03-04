{-# LANGUAGE TypeOperators #-}
module Util ( numericalDiff
            , genGrad
            , numericalGradient
            , gradientDescent
            -- plot
            , plot
            , plots
            -- others
            , rowCount
            , colCount
            ) where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Vector.Unboxed.Base
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

-- calculate numerical difference
numericalDiff :: Fractional a => (a -> a) -> a -> a
numericalDiff f x = (f (x+h) - f (x-h)) / (2*h)
  where
    h = 1e-4

genGrad :: Fractional a => (a -> a) -> a -> a -> a
genGrad f x = \a -> numDiff * a + b
  where
    numDiff = numericalDiff f x
    p = (x, f x)
    b = f x - numDiff * x

-- calculate numerical gradient
numericalGradient f x = R.fromFunction sh (\ix -> (f (xus R.! ix) - f (xls R.! ix)) / (2*h))
  where
    h = 1e-4
    sh = R.extent x
    (xus, xls) = (gen h x (+), gen h x (-))

gen h x op = R.fromFunction sh (R.fromFunction sh . d op)
  where
    sh = R.extent x
    d op ix ix' | ix == ix' = x R.! ix' `op` h
                | otherwise = x R.! ix'

gradientDescent :: (R.Source r e, R.Shape sh, R.Target r e, Fractional e) =>
  (R.Array R.D sh e -> e) -> e -> Int -> R.Array r sh e -> R.Array r sh e
gradientDescent f lr stepNum x = g !! stepNum
  where
    sh = R.extent x
    lr' = R.fromFunction sh (\ix -> lr)
    g = iterate (\s -> R.computeS (s R.-^ lr' R.*^ numericalGradient f s)) x

rowCount :: (R.Source r e) => R.Array r R.DIM2 e -> Int
rowCount = row . R.extent

colCount :: (R.Source r e) => R.Array r R.DIM2 e -> Int
colCount = col . R.extent

----------------------------------------------------------------
-- Utilities on Gnuplot
----------------------------------------------------------------

plot :: (RealFrac a, Tuple.C a) => (a -> a) -> (a, a) -> IO ()
plot f = plots [f]

plots :: (RealFrac a, Tuple.C a) => [a -> a] -> (a, a) -> IO ()
plots fs rng@(l, r) = plotPaths [] $ zipWith (\f -> fmap ((,) <$> id <*> f)) fs xss
  where
    points = round ((r - l)/4.0e-3)
    xss = repeat (linearScale points rng)
