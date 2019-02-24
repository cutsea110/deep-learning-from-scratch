{-# LANGUAGE TypeOperators #-}
module Util ( fromList
            ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

sizeOf :: (R.Shape sh, R.Source r e) => R.Array r sh e -> Int
sizeOf x = R.size $ R.extent x

fromList xs = R.fromListUnboxed (R.Z R.:. r R.:. c) $ concat xs
  where
    (r, c) = (length xs, length (head xs))

adjust :: (R.Shape sh1, R.Shape sh2, R.Source r1 a1, R.Source r2 a2) =>
          R.Array r1 sh1 a1
       -> R.Array r2 sh2 a2
       -> ((R.Array R.D (R.Z R.:. Int) a1, R.Z R.:. Int, Int),
           (R.Array R.D (R.Z R.:. Int) a2, R.Z R.:. Int, Int))
adjust x1 x2 = ((f1, sh1, s1), (f2, sh2, s2))
  where
    (s1, s2) = (sizeOf x1, sizeOf x2)
    lcm' = lcm s1 s2
    (t1, t2) = (lcm' `div` s1, lcm' `div` s2)
    sh = R.Z R.:. lcm'
    (sh1, sh2) = (R.Z R.:. s1, R.Z R.:. s2)
    f1 = R.fromFunction sh (\ix -> (R.reshape sh1 x1) R.! (R.Z R.:. (R.size ix `mod` s1)))
    f2 = R.fromFunction sh (\ix -> (R.reshape sh2 x2) R.! (R.Z R.:. (R.size ix `mod` s2)))

x1 $+$ x2 = R.reshape sh $ x1' R.+^ x2'
  where
    sh = if s1 >= s2 then sh1 else sh2
    ((x1', sh1, s1), (x2', sh2, s2)) = adjust x1 x2



x :: R.Array R.U (R.Z R.:. Int) Double
x =  R.fromListUnboxed (R.Z R.:. 2) [1.0, 0.5]
x1 :: R.Array R.U (R.Z R.:. Int) Int
x1 = R.fromListUnboxed (R.Z R.:. 3) [0..2]
x2 :: R.Array R.U (R.Z R.:. Int R.:. Int) Int
x2 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2) [0..5]

w1 :: R.Array R.U (R.Z R.:. Int R.:. Int) Double
w1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 3) [0.1,0.2,0.3,0.4,0.5,0.6]
b1 :: R.Array R.U (R.Z R.:. Int) Double
b1 = R.fromListUnboxed (R.Z R.:. 3) [0.7,0.8,0.9]
