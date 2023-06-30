
# Maps are based on src_cpp/opennnBridge/definitionsNN.h
LayerTypeMap = {
    "Default" : "0",
    "Scaling none" : "1-1",
    "Scaling MinMax" : "1-2",
    "Scaling MeanStd" : "1-3",
    "Scaling STD" : "1-4",
    "Scaling Log" : "1-5",
    "CNN" : "2",
    "Perceptron" : "3",
    "Pooling none" : "4-1",
    "Pooling max" : "4-2",
    "Pooling avg" : "4-3",
    "Probabilistic" : "5",
    "LSTM" : "6",
    "RNN" : "7",
    "Unscaling" : "8",
    "Bounding" : "9"
}

ScalingMethodMap = {
    "none" : "1",
    "MinMax" : "2",
    "MeanStd" : "3",
    "STD" : "4",
    "Log" : "5",
}

PoolingMethodMap = {
    "none" : "1",
    "max" : "2",
    "avg" : "3",
}

ActivationFunctionsMap = {
    "Threshold" : 1,
    "Sign" : 2,
    "Logistic" : 3,
    "Tanh" : 4,
    "Linear" : 5,
    "ReLU" : 6,
    "eLU" : 7,
    "SeLU" : 8,
    "Soft-plus" : 9,
    "Soft-sign" : 10,
    "Hard-sigmoid" : 11,
}

ModelTypeMapping = {
    "approximation" : 1,
    "classification" : 2,
    "forecasting" : 3,
    "encoder_decoder" : 4,
    "nn" : 5,
    "autoencoder" : 6,
    "ae-classifier" : 7,
    "fed-client": 8,
    "fed-server": 9
}

OptimizerTypeMapping = {
    "none" : 0,
    "SGD" : 1,
    "Mini-Batch" : 2,
    "Momentum" : 3,
    "NAG" : 4,
    "Adagrad" : 5,
    "ADAM" : 6
}

LossMethodMapping = {
    "SSE" : 1, # Sum squared Error
    "MSE" : 2, # Mean Squared Error
    "NSE" : 3, # Normalized Squared Error
    "Minkowski-E" : 4, # Minkowski Error
    "WSE" : 5, # Weighted Squared Error
    "CEE" : 6, # Cross Entropy Error
}

def doc_print_dict(d):#define d
    pretty_dict = ''  #take empty string
    for k, v in d.items():#get items for dict
        pretty_dict += f' {k}:{str(v)} |'
    return pretty_dict#return result


KEY_DOC_PREFIX = "_doc_"
KEY_MODEL_TYPE = "modelType"
KEY_MODEL_TYPE_DOC = "_doc_modelType"
KEY_LAYER_SIZES_LIST = "layersSizes"
KEY_LAYER_SIZES_DOC = "_doc_layersSizes"
KEY_LAYER_TYPES_LIST = "layerTypesList"
KEY_LAYER_TYPES_DOC = "_doc_LayerTypes"
KEY_SCALING_METHOD = "scalingMethod"
KEY_SCALING_METHOD_DOC = "_doc_scalingMethod"
KEY_POOLING_LAYER = "poolingMethod"
KEY_POOLING_LAYER_DOC = "_doc_poolingMethod"
KEY_LAYERS_ACTIVATION_FUNCTIONS = "layersActivationFunctions"
KEY_LAYERS_ACTIVATION_FUNCTIONS_DOC = "_doc_layersActivationFunctions"
KEY_LOSS_METHOD = "lossMethod"
KEY_LOSS_METHOD_DOC = "_doc_lossMethod"
KEY_LEARNING_RATE = "lr"
KEY_LEARNING_RATE_DOC = "_doc_lr"
KEY_OPTIMIZER_TYPE = "optimizer"
KEY_OPTIMIZER_TYPE_DOC = "_doc_optimizer"

VAL_MODEL_TYPE_DOC = f"{doc_print_dict(ModelTypeMapping)}"
VAL_LAYER_SIZES_DOC = "List of postive integers [L0, L1, ..., LN]"
VAL_LAYER_TYPES_DOC = f"{doc_print_dict(LayerTypeMap)}"
VAL_SCALING_METHOD_DOC = f"{doc_print_dict(ScalingMethodMap)}"
VAL_POOLING_METHOD_DOC = f"{doc_print_dict(PoolingMethodMap)}"
VAL_LAYERS_ACTIVATION_FUNCTIONS_DOC = f"{doc_print_dict(ActivationFunctionsMap)}"
VAL_LOSS_METHOD_DOC = f"{doc_print_dict(LossMethodMapping)}"
VAL_LEARNING_RATE_DOC = "Positve float"
VAL_OPTIMIZER_TYPE_DOC = f"{doc_print_dict(OptimizerTypeMapping)}"
