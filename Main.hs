module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.Repa as R
import Data.List (foldl')
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Algorithms.Randomish

import Perceptron
import Neuron (sigmoidFunction, forward)
import Util as U

main :: IO ()
main = do
  putStrLn "This is sample of Repa."
  print $ forward x initNetwork

x :: R.Array R.U R.DIM2 Double
x = fromList [[1.0, 0.5]]

initNetwork = [ (sigmoidFunction, w1, b1)
              , (sigmoidFunction, w2, b2)
              , (id, w3, b3)
              ]
  where
    w1, w2, w3 :: R.Array R.U R.DIM2 Double
    w1 = fromList [[0.1, 0.3, 0.5], [0.2, 0.4, 0.6]]
    w2 = fromList [[0.1, 0.4], [0.2, 0.5], [0.3, 0.6]]
    w3 = fromList [[0.1, 0.3], [0.2, 0.4]]
    
    b1, b2, b3 :: R.Array R.U R.DIM2 Double
    b1 = fromList [[0.1, 0.2, 0.3]]
    b2 = fromList [[0.1, 0.2]]
    b3 = fromList [[0.1, 0.2]]
