{-# LANGUAGE FlexibleContexts, BangPatterns #-}
{-# OPTIONS_GHC -fllvm #-}
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
import Data.Array.Repa.Eval
import Data.Array.Repa.Unsafe
import Data.Array.Repa.Algorithms.Matrix hiding(mmultS)
import Debug.Trace

import Activation

-- | Matrix matrix multiply, sequentially.
mmultS  :: R.Array R.U R.DIM2 Double
        -> R.Array R.U R.DIM2 Double
        -> R.Array R.U R.DIM2 Double
mmultS !arr !brr = R.deepSeqArray ret ret
    where
        (R.Z R.:. h1  R.:. _)  = R.extent arr
        (R.Z R.:. _   R.:. w2) = R.extent brr
        !trr = transpose2S brr
        ret = computeS
             $ R.fromFunction (R.Z R.:. h1 R.:. w2)
             $ \ix   -> R.sumAllS
                      $ R.zipWith (*)
                            (unsafeSlice arr (R.Any R.:. row ix R.:. R.All))
                            (unsafeSlice trr (R.Any R.:. col ix R.:. R.All))
{-# NOINLINE mmultS #-}

-- h is activation function
generateP (w1, w2, bias, h) x1 x2 = do
  s <- R.sumAllP (R.zipWith (*) x w)
  return $ h (bias + s)
  where
    x = R.fromListUnboxed (R.ix1 2) [x1, x2]
    w = R.fromListUnboxed (R.ix1 2) [w1, w2]

generate (w1, w2, bias, h) x1 x2 = h (bias + R.sumAllS (R.zipWith (*) x w))
  where
    x = R.fromListUnboxed (R.ix1 2) [x1, x2]
    w = R.fromListUnboxed (R.ix1 2) [w1, w2]
    

tripleP x (f, w, b) = do
  xw <- mmultP x w
  let (R.Z R.:. r R.:. c) = R.extent xw
  R.computeP $ R.map f $ xw R.+^  R.extend (R.Z R.:. r R.:. R.All) b

{-# INLINE tripleS #-}
tripleS x (f, w, b) = R.computeS $ R.map f $ xw R.+^ R.extend (R.Z R.:. r R.:. R.All) b
  where
    xw = mmultS x w
    (R.Z R.:. r R.:. c) = R.extent xw

{-# INLINE forwardS #-}
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
