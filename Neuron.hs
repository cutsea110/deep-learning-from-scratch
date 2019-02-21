{-# LANGUAGE FlexibleContexts #-}
module Neuron ( generate
              , generateP
              , triple
              , tripleP
              , forward
              , forwardP
              -- functions like as sigmoid
              , perceptronFunction
              , stepFunction
              , sigmoidFunction
              , reluFunction
              ) where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

import Util (fromList)

-- h is activation function
generateP (w1, w2, bias, h) = neuronP
  where
    neuronP x1 x2 = tmp
      where
        x, w :: R.Array R.U R.DIM2 Double
        x = fromList [[x1, x2]]
        w = fromList [[w1, w2]]
        tmp = do
          t <- R.computeUnboxedP (R.zipWith (*) x w)
          s <- R.sumAllP t
          return $ h (bias + s)

generate (w1, w2, bias, h) x1 x2 = runST (generateP (w1, w2, bias, h) x1 x2)

perceptronFunction x | x <= 0    = 0
                     | otherwise = 1

stepFunction x | x > 0     = 1
               | otherwise = 0

sigmoidFunction x = 1 / (1 + exp (-x))

reluFunction = max 0.0

tripleP x (f, w, b) = do
  xw <- mmultP x w
  R.computeUnboxedP $ R.map f $ R.zipWith (+) xw b

triple x (f, w, b) = runST (tripleP x (f, w, b))

forward :: (Foldable t, R.Source r Double) =>
  R.Array R.U R.DIM2 Double
  -> t (Double -> Double, R.Array R.U R.DIM2 Double, R.Array r R.DIM2 Double)
  -> R.Array R.U R.DIM2 Double
forward = foldl' triple

forwardP :: (Monad m, Foldable t, R.Source r Double) =>
  R.Array R.U R.DIM2 Double
  -> t (Double -> Double, R.Array R.U R.DIM2 Double, R.Array r R.DIM2 Double)
  -> m (R.Array R.U R.DIM2 Double)
forwardP = foldM tripleP
