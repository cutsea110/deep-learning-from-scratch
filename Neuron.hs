{-# LANGUAGE FlexibleContexts #-}
module Neuron ( generate
              , forward
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
import Data.Array.Repa.Algorithms.Randomish

import Util (fromList)

-- h is activation function
generate (w1, w2, bias, h) = neuron
  where
    neuron x1 x2 = h tmp
      where
        x, w :: R.Array R.U R.DIM2 Double
        x = fromList [[x1, x2]]
        w = fromList [[w1, w2]]
        tmp = bias + R.sumAllS (R.computeUnboxedS (R.zipWith (*) x w))
        {--
        tmp = runST $ do
          t <- mmultP x w
          s <- R.sumAllP t
          return $ bias + s
        --}

perceptronFunction x | x <= 0    = 0
                     | otherwise = 1

stepFunction x | x > 0     = 1
               | otherwise = 0

sigmoidFunction x = 1 / (1 + exp (-x))

reluFunction = max 0.0

triple x (f, w, b) = runST $ do
  xw <- mmultP x w
  R.computeUnboxedP $ R.zipWith ((f.).(+)) xw b

forward :: (Foldable t, R.Source r Double) =>
  R.Array R.U R.DIM2 Double
  -> t (Double -> Double, R.Array R.U R.DIM2 Double, R.Array r R.DIM2 Double)
  -> R.Array R.U R.DIM2 Double
forward = foldl' triple
