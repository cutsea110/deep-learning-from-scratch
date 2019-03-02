{-# LANGUAGE FlexibleContexts #-}
module Neuron ( generate
              , generateP
              , tripleS
              , tripleP
              , forwardS
              , forwardP
              ) where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

import Activation
import Util ((+#))

-- h is activation function
generateP (w1, w2, bias, h) = neuronP
  where
    neuronP x1 x2 = tmp
      where
        x, w :: R.Array R.U R.DIM1 Double
        x = R.fromListUnboxed (R.ix1 2) [x1, x2]
        w = R.fromListUnboxed (R.ix1 2) [w1, w2]
        tmp = do
          t <- R.computeUnboxedP (R.zipWith (*) x w)
          s <- R.sumAllP t
          return $ h (bias + s)

generate (w1, w2, bias, h) x1 x2 = runST (generateP (w1, w2, bias, h) x1 x2)

tripleP x (f, w, b) = do
  xw <- mmultP x w
  R.computeUnboxedP $ R.map f $ xw +# b

tripleS x (f, w, b) = R.computeUnboxedS $ R.map f $ mmultS x w +# b

forwardS :: (Foldable t, R.Source r Double) =>
  R.Array R.U R.DIM2 Double
  -> t (Double -> Double, R.Array R.U R.DIM2 Double, R.Array r R.DIM1 Double)
  -> R.Array R.U R.DIM2 Double
forwardS = foldl' tripleS

forwardP :: (Monad m, Foldable t, R.Source r Double) =>
  R.Array R.U R.DIM2 Double
  -> t (Double -> Double, R.Array R.U R.DIM2 Double, R.Array r R.DIM1 Double)
  -> m (R.Array R.U R.DIM2 Double)
forwardP = foldM tripleP
