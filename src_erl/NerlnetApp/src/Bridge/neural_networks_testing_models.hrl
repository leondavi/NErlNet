

-define(PERCEPTRON_TESTING_NN,{ _ModelId  = erlang:unique_integer([positive]),
                                _ModelType = "0",
                                _LayersSizes = "5,30,5,3",
                                _LayersTypes = "1,3,3,3",
                                _LayersFunctionalityCodes = "1,6,6,6", % change scaler functionality to 6 to check exception handling
                                _LearningRate = "0.01",
                                _Epochs = "5000",
                                _OptimizerType = "5",
                                _OptimizerArgs = "",
                                _LossMethod = "2",
                                _DistributedSystemType = "0",
                                _DistributedSystemArg = ""} ).

-define(CNN_TESTING_NN,{        _ModelIdCNN  = erlang:unique_integer([positive]),
                                _ModelTypeCNN = "0",
                                _LayersSizesCNN = "7x7x1k5x5p2s1,9,9,9",
                                _LayersTypesCNN = "2,3,3,3",
                                _LayersFunctionalityCodesCNN = "1,6,11,11", % change scaler functionality to 6 to check exception handling
                                _LearningRate = "0.01",
                                _EpochsCNN = "50",
                                _OptimizerTypeCNN = "5",
                                _OptimizerArgsCNN = "",
                                _LossMethodCNN = "2",
                                _DistributedSystemTypeCNN = "0",
                                _DistributedSystemArgCNN = ""} ).

-define(AEC_TESTING_NN,{        _ModelIdAEC  = erlang:unique_integer([positive]),
                                _ModelTypeAEC = "9",
                                _LayersSizesAEC = "32,16,8,4,8,16,32",
                                _LayersTypesAEC = "1,3,3,3,3,3,1",
                                _LayersFunctionalityCodesAEC = "1,11,11,11,11,11,1", 
                                _LearningRateAEC = "0.01",
                                _EpochsAEC = "50",
                                _OptimizerTypeAEC = "5",
                                _OptimizerArgsAEC = "",
                                _LossMethodAEC = "2",
                                _DistributedSystemTypeAEC = "0",
                                _DistributedSystemArgAEC = ""} ).

-define(AE_TESTING_NN, {        _ModelIdAE  = erlang:unique_integer([positive]),
                                _ModelTypeAE = "8",
                                _LayersSizesAE = "32,16,8,4,8,16,32",
                                _LayersTypesAE = "1,3,3,3,3,3,3",
                                _LayersFunctionalityCodesAE = "1,11,11,11,11,11,11", 
                                _LearningRateAE = "0.01",
                                _EpochsAE = "50",
                                _OptimizerTypeAE = "5",
                                _OptimizerArgsAE = "",
                                _LossMethodAE = "2",
                                _DistributedSystemTypeAE = "0",
                                _DistributedSystemArgAE = ""} ).

-define(NEURAL_NETWORK_TESTING_MODELS_LIST, [?PERCEPTRON_TESTING_NN , ?AE_TESTING_NN]).