{-# LANGUAGE TypeOperators #-}
module Util ( adjust
            -- operators broadcast version
            , (+#)
            , (-#)
            , (*#)
            , (/#)
            ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

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

(+#) :: (R.Source r1 c, R.Source r2 c, R.Shape sh1, R.Shape sh2,
          R.Shape sh, Num c) => R.Array r1 sh1 c -> R.Array r2 sh2 c -> R.Array R.D sh c
(+#) = (uncurry (R.+^) .) . adjust

(-#) :: (R.Source r1 c, R.Source r2 c, R.Shape sh1, R.Shape sh2,
          R.Shape sh, Num c) => R.Array r1 sh1 c -> R.Array r2 sh2 c -> R.Array R.D sh c
(-#) = (uncurry (R.-^) .) . adjust

(*#) :: (R.Source r1 c, R.Source r2 c, R.Shape sh1, R.Shape sh2,
          R.Shape sh, Num c) => R.Array r1 sh1 c -> R.Array r2 sh2 c -> R.Array R.D sh c
(*#) = (uncurry (R.*^) .) . adjust

(/#) :: (R.Source r1 c, R.Source r2 c, R.Shape sh1, R.Shape sh2,
          R.Shape sh, Fractional c) => R.Array r1 sh1 c -> R.Array r2 sh2 c -> R.Array R.D sh c
(/#) = (uncurry (R./^) .) . adjust
