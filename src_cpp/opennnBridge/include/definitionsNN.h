#pragma once 

enum ModuleType {APPROXIMATION = 1, CLASSIFICATION = 2, FORECASTING = 3 , ENCODER_DECODER = 4, CUSTUMNN = 5};

enum ScalingMethods {NoScaling = 1 , MinimumMaximum = 2 , MeanStandardDeviation = 3 , StandardDeviation = 4 , Logarithm = 5};
   
enum ActivationFunction {Threshold = 1, SymmetricThreshold = 2 , Logistic = 3 , HyperbolicTangent = 4 ,
                         Linear = 5 , RectifiedLinear = 6 , ExponentialLinear = 7 , ScaledExponentialLinear = 8 ,
                         SoftPlus = 9 , SoftSign = 10 , HardSigmoid = 11 , Binary = 12 , Competitive = 14 , Softmax = 15 };

//enum general functions Softmax , Competitive , Binary

enum LayerType {E_LAYER_TYPE_DEFAULT = 0, E_LAYER_TYPE_SCALING = 1, E_LAYER_TYPE_CONVOLUTIONAL = 2 , E_LAYER_TYPE_PERCEPTRON = 3 , E_LAYER_TYPE_POOLING = 4 , E_LAYER_TYPE_PROBABILISTIC = 5 ,
                E_LAYER_TYPE_LSTM = 6 , E_LAYER_TYPE_RECURRENT = 7 , E_LAYER_TYPE_UNSCALING = 8 , E_LAYER_TYPE_BOUNDING = 9 };
    
enum LossMethod {E_LOSS_METHOD_SUM_SQUARED_ERROR = 1, E_LOSS_METHOD_MSE = 2 /* MSE - Mean Squared Error */, E_LOSS_METHOD_NSE = 3 /* NSE - Normalized Squared Error */, 
                         E_LOSS_METHOD_MINKOWSKI_ERROR = 4, E_LOSS_METHOD_WSE = 5 /* WSE - Weighted Squared Error */, E_LOSS_METHOD_CEE = 6 /* CEE - Cross Entropy Error */};

enum OptimizationMethod {E_OM_GRADIENT_DESCENT = 1, E_OM_CONJUGATE_GRADIENT = 2, E_OM_QUASI_NEWTON_METHOD = 3 ,
                         E_OM_LEVENBERG_MARQUARDT_ALGORITHM = 4, E_OM_STOCHASTIC_GRADIENT_DESCENT = 5 , E_OM_ADAPTIVE_MOMENT_ESTIMATION = 6};

enum LearningRateAlgorithm {E_LRA_GOLDEN_SECTION = 1 /* LRA -Learning Rate Algorithm */,E_LRA_BRENT_METHOD = 2}; 
