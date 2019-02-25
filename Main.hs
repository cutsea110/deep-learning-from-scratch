module Main where

import qualified Data.Array.Repa as R

import Activation (sigmoid)
import Mnist (downloadMnist, loadTrain, loadTest, draw)
import Neuron (forwardS, forwardP)

prepareMnist = do
  downloadMnist

  (xl, xi) <- loadTrain
  (tl, ti) <- loadTest

  -- TODO: store Array as Pickle like

  print "Done."

main :: IO ()
main = do
  putStrLn "forwardS..."
  print $ forwardS x initNetwork
  putStrLn "forwardP..."
  s <- forwardP x initNetwork
  print s

x :: R.Array R.U R.DIM2 Double
x = R.fromListUnboxed (R.Z R.:.1 R.:.2) [1.0, 0.5]

initNetwork = [ (sigmoid, w1, b1)
              , (sigmoid, w2, b2)
              , (id, w3, b3)
              ]
  where
    w1,w2,w3 :: R.Array R.U R.DIM2 Double
    w1 = R.fromListUnboxed (R.Z R.:.2 R.:.3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
    w2 = R.fromListUnboxed (R.Z R.:.3 R.:.2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
    w3 = R.fromListUnboxed (R.Z R.:.2 R.:.2) [0.1, 0.3, 0.2, 0.4]

    b1,b2,b3 :: R.Array R.U R.DIM1 Double
    b1 = R.fromListUnboxed (R.Z R.:.3) [0.1, 0.2, 0.3]
    b2 = R.fromListUnboxed (R.Z R.:.2) [0.1, 0.2]
    b3 = R.fromListUnboxed (R.Z R.:.2) [0.1, 0.2]
