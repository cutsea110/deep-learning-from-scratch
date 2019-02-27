{-# LANGUAGE FlexibleContexts #-}
module Activation ( perceptron
                  , sigmoid
                  , step
                  , relu
                  , softmaxS
                  , softmaxP
                  ) where

import qualified Data.Array.Repa as R

perceptron x | x <= 0    = 0
             | otherwise = 1

step x | x > 0     = 1
       | otherwise = 0

sigmoid x = 1 / (1 + exp (-x))

relu = max 0.0

softmaxS xs = R.computeUnboxedS $ R.map (/ttl) xs'
  where
    maxVal = (R.foldS max (-1/0) xs) R.! (R.Z R.:.0)
    (xs', ttl) = (R.map (exp . subtract maxVal) xs, R.sumAllS xs')

softmaxP xs = do
  mv <- R.foldP max (-1/0) xs
  let maxVal = mv R.! (R.Z R.:.0)
  xs' <- R.computeUnboxedP $ R.map (exp . subtract maxVal) xs
  let ttl = R.sumAllS xs'
  R.computeUnboxedP $ R.map (/ttl) xs'
