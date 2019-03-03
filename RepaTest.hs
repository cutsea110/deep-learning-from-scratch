module RepaTest where

import Data.Array.Repa

xs :: Array U DIM2 Int
xs = fromListUnboxed (ix2 2 2) [1..4]

col0 = computeUnboxedS $ slice xs (Any :. (0 :: Int))
col1 = computeUnboxedS $ slice xs (Any :. (1 :: Int))

row0 = computeUnboxedS $ slice xs (Z :. (0::Int) :. All)
row1 = computeUnboxedS $ slice xs (Z :. (1::Int) :. All)
