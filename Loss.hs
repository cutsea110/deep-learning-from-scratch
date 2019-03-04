{-# LANGUAGE FlexibleContexts #-}
module Loss ( mseS
            , mseP
            , ceeS
            , ceeP
            ) where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed

-- | mean squared error
mseS :: (Fractional a, R.Shape sh, R.Source r1 a, R.Source r2 a) =>
  R.Array r1 sh a -> R.Array r2 sh a -> a
mseS y t = 0.5 * (R.sumAllS $ R.map (^2) $ y R.-^ t)

-- | mean squared error
mseP :: (Monad m, R.Shape sh, Data.Vector.Unboxed.Unbox b, R.Source r1 b, R.Source r2 b, Fractional b) =>
  R.Array r1 sh b -> R.Array r2 sh b -> m b
mseP y t = do
  x <- R.sumAllP $ R.map (^2) $ y R.-^ t
  return (0.5 * x)

-- | cross entropy error
ceeS :: (R.Shape sh, R.Source r a, R.Source r1 a, Floating a) =>
  R.Array r sh a -> R.Array r1 sh a -> a
ceeS y t = negate (R.sumAllS $ t R.*^ R.map (log . (+delta)) y) / (fromIntegral batchSize)
  where
    delta = 1e-7
    batchSize = last $ R.listOfShape $ R.extent y

-- | cross entropy error
ceeP :: (R.Shape sh, Monad m, Data.Vector.Unboxed.Unbox b, R.Source r b, R.Source r1 b, Floating b) =>
  R.Array r sh b -> R.Array r1 sh b -> m b
ceeP y t = do
  x <- R.sumAllP $ t R.*^ R.map (log . (+delta)) y
  return (negate x / fromIntegral batchSize)
  where
    delta = 1e-7
    batchSize = last $ R.listOfShape $ R.extent y
