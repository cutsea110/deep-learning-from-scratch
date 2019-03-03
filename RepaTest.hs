module RepaTest where

import Data.Array.Repa

xs :: Array U DIM2 Int
xs = fromListUnboxed (ix2 2 3) [1..6]

col0 = computeUnboxedS $ slice xs (Any :. (0 :: Int))
col1 = computeUnboxedS $ slice xs (Any :. (1 :: Int))
col2 = computeUnboxedS $ slice xs (Any :. (2 :: Int))

row0 = computeUnboxedS $ slice xs (Z :. (0::Int) :. All)
row1 = computeUnboxedS $ slice xs (Z :. (1::Int) :. All)

colextend2 = computeUnboxedS $ extend (Any :.(2::Int)) xs
colextend3 = computeUnboxedS $ extend (Any :.(3::Int)) xs

rowextend2 = computeUnboxedS $ extend (Any :. (2::Int) :. All) xs
rowextend3 = computeUnboxedS $ extend (Any :. (3::Int) :. All) xs
