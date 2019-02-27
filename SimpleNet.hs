{-# LANGUAGE FlexibleContexts #-}
module SimpleNet where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

import Activation (softmaxS, softmaxP)
import Loss (ceeS, ceeP)
import Util

w :: R.Array R.U R.DIM2 Double
w = R.fromListUnboxed (R.Z R.:.2 R.:.3) [0.47355232, 0.9977393, 0.84668094,
                                         0.85557411, 0.03563661, 0.69422093]

x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.Z R.:.1 R.:.2) [0.6, 0.9]

t :: R.Array R.U R.DIM2 Double
t = R.fromListUnboxed (R.Z R.:.1 R.:.3) [0, 0, 1]

predict x = mmultS x w

loss x t = ceeS (softmaxS (predict x)) t

f w = loss x t
