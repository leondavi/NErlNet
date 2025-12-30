-define(TORCH_TEST_MODEL_PERCEPTRON_RELATIVE_PATH, "tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt").
-define(TORCH_TEST_MODEL_PERCEPTRON_DEFAULT_PATH, "/usr/local/lib/nerlnet-lib/NErlNet/tests/inputTorchJsonsFiles/models/placeholder_perceptron.pt").

-define(TORCH_TEST_MODEL_AUTOENCODER_RELATIVE_PATH, "tests/inputTorchJsonsFiles/models/placeholder_autoencoder.pt").
-define(TORCH_TEST_MODEL_AUTOENCODER_DEFAULT_PATH, "/usr/local/lib/nerlnet-lib/NErlNet/tests/inputTorchJsonsFiles/models/placeholder_autoencoder.pt").

-define(TORCH_TEST_MODEL_CNN_RELATIVE_PATH, "tests/inputTorchJsonsFiles/models/placeholder_cnn.pt").
-define(TORCH_TEST_MODEL_CNN_DEFAULT_PATH, "/usr/local/lib/nerlnet-lib/NErlNet/tests/inputTorchJsonsFiles/models/placeholder_cnn.pt").

%% Legacy aliases kept for backwards compatibility
-define(TORCH_TEST_MODEL_RELATIVE_PATH, ?TORCH_TEST_MODEL_PERCEPTRON_RELATIVE_PATH).
-define(TORCH_TEST_MODEL_DEFAULT_PATH, ?TORCH_TEST_MODEL_PERCEPTRON_DEFAULT_PATH).

-define(PERCEPTRON_TESTING_NN,{ _ModelId  = erlang:unique_integer([positive]),
                                _ModelType = "0",
                                   _ModelArgs = ?TORCH_TEST_MODEL_PERCEPTRON_RELATIVE_PATH,
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

-define(PERCEPTRON_TESTING_DISTRIBUTED_NN,{ _ModelId  = erlang:unique_integer([positive]),
                                _ModelType = "0",
                                _ModelArgs = ?TORCH_TEST_MODEL_PERCEPTRON_RELATIVE_PATH,
                                _LayersSizes = "5,30,5,3",
                                _LayersTypes = "1,3,3,3",
                                _LayersFunctionalityCodes = "1,6,6,6", % change scaler functionality to 6 to check exception handling
                                _LearningRate = "0.01",
                                _Epochs = "50",
                                _OptimizerType = "2",
                                _OptimizerArgs = "",
                                _LossMethod = "2",
                                _DistributedSystemType = "0", % TODO Ori put the correct value
                                _DistributedSystemArg = ""} ).

-define(CNN_TESTING_NN,{        _ModelIdCNN  = erlang:unique_integer([positive]),
                                _ModelTypeCNN = "0",
                                _ModelArgsCNN = ?TORCH_TEST_MODEL_CNN_RELATIVE_PATH,
                                _LayersSizesCNN = "28x28x1k5x5x1x6p0s1t1,28x28x6k2x2p0s2,14x14x6k4x4x6x12p0s1t0,1,32,10",
                                _LayersTypesCNN = "2,4,2,9,3,5",
                                _LayersFunctionalityCodesCNN = "6,2,6,6,6,4", % change scaler functionality to 6 to check exception handling
                                _LearningRateCNN = "0.01",
                                _EpochsCNN = "50",
                                _OptimizerTypeCNN = "5",
                                _OptimizerArgsCNN = "",
                                _LossMethodCNN = "2",
                                _DistributedSystemTypeCNN = "0",
                                _DistributedSystemArgCNN = ""} ).

-define(AEC_TESTING_NN,{        _ModelIdAEC  = erlang:unique_integer([positive]),
                                _ModelTypeAEC = "9",
                                _ModelArgsAEC = ?TORCH_TEST_MODEL_AUTOENCODER_RELATIVE_PATH,
                                _LayersSizesAEC = "32,16,8,4,8,16,32,32", % last layer (perceptron) should be the same as the input layer , followed by bounding layer
                                _LayersTypesAEC = "1,3,3,3,3,3,3,10",
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
                                _ModelArgsAE = ?TORCH_TEST_MODEL_AUTOENCODER_RELATIVE_PATH,
                                _LayersSizesAE = "32,16,8,4,8,16,32,32", % last layer (perceptron) should be the same as the input layer , followed by bounding layer
                                _LayersTypesAE = "1,3,3,3,3,3,3,10",
                                _LayersFunctionalityCodesAE = "1,11,11,11,11,11,11,1", 
                                _LearningRateAE = "0.01",
                                _EpochsAE = "50",
                                _OptimizerTypeAE = "5",
                                _OptimizerArgsAE = "",
                                _LossMethodAE = "2",
                                _DistributedSystemTypeAE = "0",
                                _DistributedSystemArgAE = ""} ).

-define(NEURAL_NETWORK_TESTING_MODELS_LIST, [?PERCEPTRON_TESTING_NN ,?AEC_TESTING_NN , ?CNN_TESTING_NN]).
-define(NEURAL_NETWORK_TESTING_MODELS_LIST_NAMES, ["Perceptron" ,"AEC" ,"CNN"]).