{-# LANGUAGE TypeOperators #-}
module Util ( mmult
            , genMaps -- test
              -- numerical gradient etc.
            , numericalDiff
            , genGrad
            , numericalGradient
            , gradientDescent
              -- plot
            , plot
            , plots
            ) where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Operators.IndexSpace (unsafeSlice)
import Data.Vector.Unboxed.Base
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

-- mmult over D
mmult :: (R.Source r1 a, R.Source r2 a, Num a) =>
  R.Array r2 R.DIM2 a -> R.Array r1 R.DIM2 a -> R.Array R.D R.DIM2 a
mmult arr brr = do
  let trr = R.transpose brr
  let (R.Z R.:. h R.:. _) = R.extent arr
      (R.Z R.:. _ R.:. w) = R.extent brr
  R.fromFunction (R.ix2 h w)
    $ \ix -> R.sumAllS $ R.zipWith (*)
             (unsafeSlice arr (R.Any R.:. (row ix) R.:. R.All))
             (unsafeSlice trr (R.Any R.:. (col ix) R.:. R.All))

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
genMaps :: (R.Shape sh, R.Source r e, Unbox e, Fractional e) =>
  R.Array r sh e -> (R.Array R.U sh e, R.Array R.U sh e)
genMaps x = (xu', xl')
  where
    h = 1e-4
    (xu, xl) = (R.map (+h) x, R.map (\x -> x - h) x)
    (xu', xl') = (R.computeUnboxedS xu, R.computeUnboxedS xl)

ng :: (R.Shape sh, R.Source r e, Unbox e, Fractional e, Fractional a) =>
  (R.Array R.D sh e -> a) -> R.Array r sh e -> R.Array R.D sh a
ng f x = R.fromFunction sh (\ix -> (f (fu ix) - f (fl ix)) / h2)
  where
    (h,h2) = (1e-4, 2*h)
    sh = R.extent x
    (xu, xl) = genMaps x
    fu tix = R.fromFunction sh (\ix -> if ix == tix then xu R.! ix else x R.! ix)
    fl tix = R.fromFunction sh (\ix -> if ix == tix then xl R.! ix else x R.! ix)

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
