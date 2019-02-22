module Util ( fromList
            ) where

import qualified Data.Array.Repa as R

fromList xs = R.fromListUnboxed (R.Z R.:. r R.:. c) $ concat xs
  where
    (r, c) = (length xs, length (head xs))
