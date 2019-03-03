module RepaTest where

import Data.Array.Repa

-- [[1,2,3]
-- ,[4,5,6]]
xs :: Array U DIM2 Int
xs = fromListUnboxed (ix2 2 3) [1..6]

-- [1,4]
col0 = computeUnboxedS $ slice xs (Any :. (0 :: Int))
-- [2,5]
col1 = computeUnboxedS $ slice xs (Any :. (1 :: Int))
-- [3,6]
col2 = computeUnboxedS $ slice xs (Any :. (2 :: Int))

-- [1,2,3]
row0 = computeUnboxedS $ slice xs (Z :. (0::Int) :. All)
-- [4,5,6]
row1 = computeUnboxedS $ slice xs (Z :. (1::Int) :. All)

-- [[[1,1],[2,2],[3,3]]
-- ,[[4,4],[5,5],[6,6]]]
colextend2 = computeUnboxedS $ extend (Any :.(2::Int)) xs
-- [[[1,1,1],[2,2,2],[3,3,3]]
-- ,[[4,4,4],[5,5,5],[6,6,6]]]
colextend3 = computeUnboxedS $ extend (Any :.(3::Int)) xs

-- [[[1,2,3]
--  ,[1,2,3]]
-- ,[[4,5,6]
--  ,[4,5,6]]]
midextend2 = computeUnboxedS $ extend (Any :. (2::Int) :. All) xs
-- [[[1,2,3]
--  ,[1,2,3]
--  ,[1,2,3]]
-- ,[[4,5,6]
--  ,[4,5,6]
--  ,[4,5,6]]]
midextend3 = computeUnboxedS $ extend (Any :. (3::Int) :. All) xs

-- [[[1,2,3]
--  ,[4,5,6]]
-- ,[[1,2,3]
--  ,[4,5,6]]]
rowextend2 = computeUnboxedS $ extend (Z :. (2::Int) :. All :. All) xs
-- [[[1,2,3]
--  ,[4,5,6]]
-- ,[[1,2,3]
--  ,[4,5,6]]
-- ,[[1,2,3]
--  ,[4,5,6]]]
rowextend3 = computeUnboxedS $ extend (Z :. (3::Int) :. All :. All) xs
