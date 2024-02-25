

-define(PERCEPTRON_TESTING_NN,{ _ModelId  = erlang:unique_integer([positive]),
                                _ModelType = "0",
                                _LayersSizes = "5,10,5,3",
                                _LayersTypes = "1,3,3,3",
                                _LayersFunctionalityCodes = "1,6,11,11", % change scaler functionality to 6 to check exception handling
                                _LearningRate = "0.01",
                                _Epochs = "50",
                                _OptimizerType = "2",
                                _OptimizerArgs = "",
                                _LossMethod = "2",
                                _DistributedSystemType = "0",
                                _DistributedSystemArg = ""} ).

-define(NEURAL_NETWORK_TESTING_MODELS_LIST, [?PERCEPTRON_TESTING_NN]).