#pragma once 

enum ModuleType {APPROXIMATION = 1, CLASSIFICATION = 2, FORECASTING = 3 , ENCODER_DECODER = 4, CUSTUMNN = 5};

enum ScalingMethods {NoScaling = 1 , MinimumMaximum = 2 , MeanStandardDeviation = 3 , StandardDeviation = 4 , Logarithm = 5};
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 , Binary = 12 , Competitive = 14 , Softmax = 15 };

enum LayerType {E_LAYER_TYPE_DEFAULT = 0, E_LAYER_TYPE_SCALING = 1, E_LAYER_TYPE_CONVOLUTIONAL = 2 , E_LAYER_TYPE_PERCEPTRON = 3 , E_LAYER_TYPE_POOLING = 4 , E_LAYER_TYPE_PROBABILISTIC = 5 ,
                E_LAYER_TYPE_LSTM = 6 , E_LAYER_TYPE_RECURRENT = 7 , E_LAYER_TYPE_UNSCALING = 8 , E_LAYER_TYPE_BOUNDING = 9 };
    
enum LossMethod {E_LOSS_METHOD_SUM_SQUARED_ERROR = 1, E_LOSS_METHOD_MSE = 2 /* MSE - Mean Squared Error */, Normalized_Squared_Error = 3 , 
                         Minkowski_Error = 4, Weighted_Squared_Error = 5 , Cross_Entropy_Error = 6};

enum OptimizationMethod {GRADIENT__DESCENT = 1, CONJUGATE__GRADIENT = 2, QUASI__NEWTON_METHOD = 3 ,
                         LEVENBERG__MARQUARDT_ALGORITHM = 4, STOCHASTIC__GRADIENT_DESCENT = 5 , ADAPTIVE__MOMENT_ESTIMATION = 6};