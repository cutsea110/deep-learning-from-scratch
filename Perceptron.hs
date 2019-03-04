module Perceptron ( pAND
                  , pNAND
                  , pOR
                  , pXOR
                  ) where

import Activation (perceptron, step, sigmoid, relu)
import Neuron (generate)

genPerceptron (w1, w2, bias) = generate (w1, w2, bias, perceptron)
genSteperon (w1, w2, bias) = generate (w1, w2, bias, step)
genNeuron (w1, w2, bias) = generate (w1, w2, bias, sigmoid)
genReLU (w1, w2, bias) = generate (w1, w2, bias, relu)

pAND  = genPerceptron (( 0.5,  0.5, -0.7) :: (Double, Double, Double))
pNAND = genPerceptron ((-0.5, -0.5,  0.7) :: (Double, Double, Double))
pOR   = genPerceptron (( 0.5,  0.5, -0.2) :: (Double, Double, Double))
pXOR x1 x2 = pAND (pNAND x1 x2) (pOR x1 x2)
