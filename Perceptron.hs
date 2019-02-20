module Perceptron where


pAND x1 x2 | tmp <= theta = 0
           | otherwise    = 1
  where
    (w1, w2, theta) = (0.5, 0.5, 0.7)
    tmp = x1 * w1 + x2 * w2

pNAND x1 x2 | tmp <= theta = 0
            | otherwise    = 1
  where
    (w1, w2, theta) = (-0.5, -0.5, -0.7)
    tmp = x1 * w1 + x2 * w2

pOR x1 x2 | tmp <= theta = 0
          | otherwise    = 1
  where
    (w1, w2, theta) = (0.5, 0.5, 0.2)
    tmp = x1 * w1 + x2 * w2

