{-# LANGUAGE TypeOperators #-}
module Util ( fromList
            -- operators like breadcast
            , (+#)
            , (-#)
            , (*#)
            , (/#)
            ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

sizeOf :: (R.Shape sh, R.Source r e) => R.Array r sh e -> Int
sizeOf x = R.size $ R.extent x

fromList xs = R.fromListUnboxed (R.Z R.:. r R.:. c) $ concat xs
  where
    (r, c) = (length xs, length (head xs))

adjust :: (R.Source r1 a1, R.Source r2 a2, R.Shape sh1, R.Shape sh2, R.Shape sh3) =>
          R.Array r1 sh1 a1 -> R.Array r2 sh2 a2 -> (R.Array R.D sh3 a1, R.Array R.D sh3 a2)
adjust x1 x2 = (f1, f2)
  where
    (sh1, sh2) = (R.extent x1, R.extent x2)
    (r1, r2) = (R.rank sh1, R.rank sh2)
    (d1, d2) = (R.listOfShape sh1, R.listOfShape sh2)
    (d1', d2') = if r1 > r2 then (d1, d2 ++ take (r1 - r2) (repeat 1)) else (d1 ++ take (r2 - r1) (repeat 1), d2)
    p a b | a > b && a `mod` b == 0 = a
          | a < b && b `mod` a == 0 = b
          | a == b                  = a
          | otherwise               = error "Unmatch dimensions."
    (dims, t1s, t2s) = (zipWith p d1' d2', zipWith div dims d1', zipWith div dims d2')
    sh = R.shapeOfList dims
    f1 = R.fromFunction sh (\ix -> x1 R.! (R.shapeOfList (take r1 (zipWith div (R.listOfShape ix) t1s))))
    f2 = R.fromFunction sh (\ix -> x2 R.! (R.shapeOfList (take r2 (zipWith div (R.listOfShape ix) t2s))))

infixl 6 +#, -#
infixl 7 *#, /#

x +# y = f R.+^ g 
  where
    (f, g) = adjust x y

x -# y = f R.-^ g 
  where
    (f, g) = adjust x y

x *# y = f R.*^ g 
  where
    (f, g) = adjust x y

x /# y = f R./^ g 
  where
    (f, g) = adjust x y

x :: R.Array R.U (R.Z R.:. Int) Double
x =  R.fromListUnboxed (R.Z R.:. 2) [1.0, 0.5]
x' :: R.Array R.U (R.Z R.:. Int R.:. Int) Int
x' =  R.fromListUnboxed (R.Z R.:. 2 R.:. 1) [10, 20]
x'' :: R.Array R.U (R.Z R.:. Int) Int
x'' =  R.fromListUnboxed (R.Z R.:. 2) [1, 2]

x1 :: R.Array R.U (R.Z R.:. Int) Int
x1 = R.fromListUnboxed (R.Z R.:. 3) [0..2]
x2 :: R.Array R.U (R.Z R.:. Int R.:. Int) Int
x2 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2) [0..5]

w1 :: R.Array R.U (R.Z R.:. Int R.:. Int) Double
w1 = R.fromListUnboxed (R.Z R.:. 2 R.:. 3) [0.1,0.2,0.3,0.4,0.5,0.6]
b1 :: R.Array R.U (R.Z R.:. Int) Double
b1 = R.fromListUnboxed (R.Z R.:. 3) [0.7,0.8,0.9]

y1 :: R.Array R.U (R.Z R.:. Int R.:. Int R.:. Int) Int
y1 = R.fromListUnboxed (R.Z R.:. 3 R.:. 2 R.:. 4) [1..24]
z1 :: R.Array R.U R.Z Double
z1 = R.fromListUnboxed R.Z [0.01]

y2 :: R.Array R.U (R.Z R.:. Int R.:. Int) Int
y2 = R.fromListUnboxed (R.Z R.:. 2 R.:. 3) [2,4,6,8,10,12]
z2 :: R.Array R.U (R.Z R.:. Int R.:. Int R.:. Int) Int
z2 = R.fromListUnboxed (R.Z R.:. 4 R.:. 1 R.:. 1) [10,100,1000,10000]
