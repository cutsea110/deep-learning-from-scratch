module Loss where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed
import Util ((+#), (*#))

-- | mean squared error
mseS :: (Fractional a, R.Shape sh, R.Source r1 a, R.Source r2 a) =>
        R.Array r1 sh a -> R.Array r2 sh a -> a
mseS y t = 0.5 * (R.sumAllS $ R.map (^2) $ y R.-^ t)

-- | mean squared error
mseP :: (Monad m, R.Shape sh, Data.Vector.Unboxed.Unbox b,
         R.Source r1 b, R.Source r2 b, Fractional b) =>
        R.Array r1 sh b -> R.Array r2 sh b -> m b
mseP y t = do
  x <- R.sumAllP $ R.map (^2) $ y R.-^ t
  return (0.5 * x)

-- | cross entropy error
ceeS y t = negate $ R.sumAllS $ t R.*^ R.map (log . (+delta)) y
  where
    delta = 1e-7

-- | cross entropy error
ceeP y t = do
  x <- R.sumAllP $ t R.*^ R.map (log . (+delta)) y
  return (negate x)
  where
    delta = 1e-7
