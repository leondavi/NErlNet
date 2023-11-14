#pragma once

// This file was auto generated
// Generated by Nerlplanner version: 1.0.0

enum LayerTypeEnum{LAYER_TYPE_DEFAULT=0,LAYER_TYPE_SCALING=1,LAYER_TYPE_CNN=2,LAYER_TYPE_PERCEPTRON=3,LAYER_TYPE_POOLING=4,LAYER_TYPE_PROBABILISTIC=5,LAYER_TYPE_LSTM=6,LAYER_TYPE_RECCURRENT=7,LAYER_TYPE_UNSCALING=8,LAYER_TYPE_BOUNDING=9};
enum ProbabilisticActivationEnum{PROBABILISTIC_ACTIVATION_BINARY=1,PROBABILISTIC_ACTIVATION_LOGISTIC=2,PROBABILISTIC_ACTIVATION_COMPETITIVE=3,PROBABILISTIC_ACTIVATION_SOFTMAX=4};
enum ScalingEnum{SCALING_NONE=1,SCALING_MINMAX=2,SCALING_MEANSTD=3,SCALING_STD=4,SCALING_LOG=5};
enum UnscalingEnum{UNSCALING_NONE=1,UNSCALING_MINMAX=2,UNSCALING_MEANSTD=3,UNSCALING_STD=4,UNSCALING_LOG=5};
enum PoolingEnum{POOLING_NONE=1,POOLING_MAX=2,POOLING_AVG=3};
enum ActivationEnum{ACTIVATION_THRESHOLD=1,ACTIVATION_SIGN=2,ACTIVATION_LOGISTIC=3,ACTIVATION_TANH=4,ACTIVATION_LINEAR=5,ACTIVATION_RELU=6,ACTIVATION_ELU=7,ACTIVATION_SELU=8,ACTIVATION_SOFT_PLUS=9,ACTIVATION_SOFT_SIGN=10,ACTIVATION_HARD_SIGMOID=11};
enum ModelTypeEnum{MODEL_TYPE_APPROXIMATION=1,MODEL_TYPE_CLASSIFICATION=2,MODEL_TYPE_FORECASTING=3,MODEL_TYPE_ENCODER_DECODER=4,MODEL_TYPE_NN=5,MODEL_TYPE_AUTOENCODER=6,MODEL_TYPE_AE_CLASSIFIER=7,MODEL_TYPE_FED_CLIENT=8,MODEL_TYPE_FED_SERVER=9};
enum OptimizerEnum{OPTIMIZER_NONE=0,OPTIMIZER_SGD=1,OPTIMIZER_MINI_BATCH=2,OPTIMIZER_MOMENTUM=3,OPTIMIZER_NAG=4,OPTIMIZER_ADAGRAD=5,OPTIMIZER_ADAM=6};
enum LossMethodEnum{LOSS_METHOD_SSE=1,LOSS_METHOD_MSE=2,LOSS_METHOD_NSE=3,LOSS_METHOD_MINKOWSKIE=4,LOSS_METHOD_WSE=5,LOSS_METHOD_CEE=6};
