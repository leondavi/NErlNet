

-define(PERCEPTRON_TESTING_NN,{ _ModelId  = erlang:unique_integer([positive]),
                                _ModelType = "0",
                                _ModelArgs = "",
                                _LayersSizes = "5,30,5,3",
                                _LayersTypes = "1,3,3,3",
                                _LayersFunctionalityCodes = "1,6,6,6", % change scaler functionality to 6 to check exception handling
                                _LearningRate = "0.01",
                                _Epochs = "50",
                                _OptimizerType = "2",
                                _OptimizerArgs = "",
                                _LossMethod = "2",
                                _DistributedSystemType = "0",
                                _DistributedSystemArg = ""} ).


-define(CNN_TESTING_NN,{        _ModelIdCNN  = erlang:unique_integer([positive]),
                                _ModelTypeCNN = "0",
                                _ModelArgsCNN = "",
                                _LayersSizesCNN = "28x28x1k5x5x1x6p0s1t1,28x28x6k2x2p0s2,14x14x6k4x4x6x12p0s1t0,1,32,10",
                                _LayersTypesCNN = "2,4,2,10,3,5",
                                _LayersFunctionalityCodesCNN = "6,2,6,6,6,4", % change scaler functionality to 6 to check exception handling
                                _LearningRateCNN = "0.01",
                                _EpochsCNN = "50",
                                _OptimizerTypeCNN = "5",
                                _OptimizerArgsCNN = "",
                                _LossMethodCNN = "2",
                                _DistributedSystemTypeCNN = "0",
                                _DistributedSystemArgCNN = ""} ).

-define(CNN_1D_TESTING_NN,{     _ModelIdCNN_1D  = erlang:unique_integer([positive]),
                                _ModelTypeCNN_1D = "0",
                                _ModelArgsCNN_1D = "",
                                _LayersSizesCNN_1D = "70x1x1k5x1x1x128p0s1t0,66x1x128k2x1p0s1,65x1x128k5x1x128x128p0s1t0,61x1x128k2x1p0s1,60x1x128k5x1x128x64p0s1t0,1,64,32,16,9",
                                _LayersTypesCNN_1D = "2,4,2,4,2,10,3,3,3,5",
                                _LayersFunctionalityCodesCNN_1D = "6,2,6,2,6,1,6,6,6,4", % change scaler functionality to 6 to check exception handling
                                _LearningRateCNN_1D = "0.01",
                                _EpochsCNN_1D = "50",
                                _OptimizerTypeCNN_1D = "5",
                                _OptimizerArgsCNN_1D = "",
                                _LossMethodCNN_1D = "2",
                                _DistributedSystemTypeCNN_1D = "0",
                                _DistributedSystemArgCNN_1D = ""} ).

-define(AEC_TESTING_NN,{        _ModelIdAEC  = erlang:unique_integer([positive]),
                                _ModelTypeAEC = "9",
                                _ModelArgsAEC = "",
                                _LayersSizesAEC = "32,16,8,4,8,16,32,32", % last layer (perceptron) should be the same as the input layer , followed by bounding layer
                                _LayersTypesAEC = "1,3,3,3,3,3,3,11",
                                _LayersFunctionalityCodesAEC = "1,11,11,11,11,11,11,1", 
                                _LearningRateAEC = "0.01",
                                _EpochsAEC = "50",
                                _OptimizerTypeAEC = "5",
                                _OptimizerArgsAEC = "",
                                _LossMethodAEC = "2",
                                _DistributedSystemTypeAEC = "0",
                                _DistributedSystemArgAEC = ""} ).

-define(AE_TESTING_NN, {        _ModelIdAE  = erlang:unique_integer([positive]),
                                _ModelTypeAE = "8",
                                _ModelArgsAE = "",
                                _LayersSizesAE = "32,16,8,4,8,16,32,32", % last layer (perceptron) should be the same as the input layer , followed by bounding layer
                                _LayersTypesAE = "1,3,3,3,3,3,3,11",
                                _LayersFunctionalityCodesAE = "1,11,11,11,11,11,11,1", 
                                _LearningRateAE = "0.01",
                                _EpochsAE = "50",
                                _OptimizerTypeAE = "5",
                                _OptimizerArgsAE = "",
                                _LossMethodAE = "2",
                                _DistributedSystemTypeAE = "0",
                                _DistributedSystemArgAE = ""} ).

-define(NEURAL_NETWORK_TESTING_MODELS_LIST, [?PERCEPTRON_TESTING_NN ,?AEC_TESTING_NN , ?CNN_TESTING_NN,?CNN_1D_TESTING_NN]).
-define(NEURAL_NETWORK_TESTING_MODELS_LIST_NAMES, ["Perceptron" ,"AEC" ,"CNN","CNN_1D"]).