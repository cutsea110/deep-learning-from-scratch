{-# LANGUAGE FlexibleContexts #-}
module SimpleNet where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

import Activation (softmaxS, softmaxP)
import Loss (ceeS, ceeP)
import Util

w :: R.Array R.U R.DIM2 Double
w = R.fromListUnboxed (R.ix2 2 3) [ 0.47355232, 0.9977393, 0.84668094
                                        , 0.85557411, 0.03563661, 0.69422093
                                        ]

x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.ix2 1 2) [0.6, 0.9]

t :: R.Array R.U R.DIM2 Double
t = R.fromListUnboxed (R.ix2 1 3) [0, 0, 1]

predictS x = mmultS x w

lossS x t = ceeS (softmaxS (predictS x)) t

f w = ceeS (softmaxS (mmultS x (R.computeUnboxedS w))) t

f' w = do
  x <- mmultP x w
  y <- softmaxP x
  ceeP y t

predictP x = mmultP x w

lossP x t = do
  z <- predictP x
  y <- softmaxP z
  ceeP y t

dW = R.computeUnboxedS $ numericalGradient f w
