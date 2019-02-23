module Main where

import Mnist (downloadMnist, loadTrain, loadTest)
import Neuron (sigmoidFunction, forward, forwardP)
import Util (fromList)

prepareMnist = do
  downloadMnist

  (xl, xi) <- loadTrain
  (tl, ti) <- loadTest

  -- TODO: store Array as Pickle like

  print "Done."

main :: IO ()
main = do
  putStrLn "forward..."
  print $ forward x initNetwork
  putStrLn "forwardP..."
  s <- forwardP x initNetwork
  print s

x = fromList [[1.0, 0.5]]

initNetwork = [ (sigmoidFunction, w1, b1)
              , (sigmoidFunction, w2, b2)
              , (id, w3, b3)
              ]
  where
    w1 = fromList [[0.1, 0.3, 0.5], [0.2, 0.4, 0.6]]
    w2 = fromList [[0.1, 0.4], [0.2, 0.5], [0.3, 0.6]]
    w3 = fromList [[0.1, 0.3], [0.2, 0.4]]

    b1 = fromList [[0.1, 0.2, 0.3]]
    b2 = fromList [[0.1, 0.2]]
    b3 = fromList [[0.1, 0.2]]
