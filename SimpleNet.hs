module SimpleNet where

import qualified Data.Array.Repa as R

w :: R.Array R.U R.DIM2 Double
w = R.fromListUnboxed (R.Z R.:.2 R.:.3) [0.47355232, 0.9977393, 0.84668094,
                                         0.85557411, 0.03563661, 0.69422093]
