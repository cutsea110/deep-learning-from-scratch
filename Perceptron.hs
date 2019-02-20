module Perceptron where

import Neuron (generate, perceptronFunction, stepFunction, sigmoidFunction, reluFunction)

genPerceptron (w1, w2, bias) = generate (w1, w2, bias, perceptronFunction)
genSteperon (w1, w2, bias) = generate (w1, w2, bias, stepFunction)
genNeuron (w1, w2, bias) = generate (w1, w2, bias, sigmoidFunction)
genReLU (w1, w2, bias) = generate (w1, w2, bias, reluFunction)

pAND  = genPerceptron ( 0.5,  0.5, -0.7)
pNAND = genPerceptron (-0.5, -0.5,  0.7)
pOR   = genPerceptron ( 0.5,  0.5, -0.2)
pXOR x1 x2 = pAND (pNAND x1 x2) (pOR x1 x2)
