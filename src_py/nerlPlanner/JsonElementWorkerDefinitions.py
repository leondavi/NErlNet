
# Maps are based on src_cpp/opennnBridge/definitionsNN.h
from collections import OrderedDict

LayerTypeMap = OrderedDict([
    ("Default" , "0"),
    ("Scaling" , "1"),
    ("CNN" , "2"),
    ("Perceptron" , "3"),
    ("Pooling" , "4"),
    ("Probabilistic" , "5"),
    ("LSTM" , "6"),
    ("Reccurrent" , "7"),
    ("Unscaling" , "8"),
    ("Bounding" , "9")]
)

ProbabilisticActivationFunctionMap = OrderedDict(
    [("Binary" , "1"),
    ("Logistic" , "2"),
    ("Competitive" , "3"),
    ("Softmax" , "4")]
)

ScalingMethodMap = OrderedDict(
    [("none" , "1"),
    ("MinMax" , "2"),
    ("MeanStd" , "3"),
    ("STD" , "4"),
    ("Log" , "5")]
)

UnScalingMethodMap = OrderedDict(
    [("none" , "1"),
    ("MinMax" , "2"),
    ("MeanStd" , "3"),
    ("STD" , "4"),
    ("Log" , "5")]
)

PoolingMethodMap = OrderedDict(
    [("none" , "1"),
    ("Max" , "2"),
    ("Avg" , "3")]
)

ActivationFunctionsMap = OrderedDict(
    [("Threshold" , "1"),
    ("Sign" , "2"),
    ("Logistic" , "3"),    
    ("Tanh" , "4"),
    ("Linear" , "5"),
    ("ReLU" , "6"),
    ("eLU" , "7"),
    ("SeLU" , "8"),
    ("Soft-plus" , "9"),
    ("Soft-sign" , "10"),
    ("Hard-sigmoid" , "11")]
)

# Maps from layer type to the functionality of layer mapping 
LayerTypeToFunctionalMap = OrderedDict([
    ("Scaling" , ScalingMethodMap),
    ("CNN" , None),
    ("Perceptron" , ActivationFunctionsMap),
    ("Pooling" , PoolingMethodMap),
    ("Probabilistic" , ProbabilisticActivationFunctionMap),
    ("LSTM" , None),
    ("Reccurrent" , None),
    ("Unscaling" , UnScalingMethodMap),
    ("Bounding" , None)]
)

ModelTypeMapping = OrderedDict([
    ("approximation" , "1"),
    ("classification" , "2"),
    ("forecasting" , "3"),
    ("encoder_decoder" , "4"),
    ("nn" , "5"),
    ("autoencoder" , "6"),
    ("ae-classifier" , "7")
])

OptimizerTypeMapping = OrderedDict([
    ("none" , "0"),
    ("SGD" , "1"),
    ("Mini-Batch" , "2"),
    ("Momentum" , "3"),
    ("NAG" , "4"),
    ("Adagrad" , "5"),
    ("ADAM" , "6")
])

LossMethodMapping = OrderedDict([
    ("SSE" , "1"), # Sum squared Error
    ("MSE" , "2"), # Mean Squared Error
    ("NSE" , "3"), # Normalized Squared Error
    ("MinkowskiE" , "4"), # Minkowski Error
    ("WSE" , "5"), # Weighted Squared Error
    ("CEE" , "6"), # Cross Entropy Error
])

DistributedSystemTypeMapping = OrderedDict([
    ("none" , "0"),
    ("fedClientAvg" , "1"),
    ("fedServerAvg" , "2")
])

InfraTypeMapping = OrderedDict([
    ("opennn" , "0"),
    ("wolfengine" , "1"),
])    

def get_key_by_value(in_map : dict, value):
    list_of_values = list(in_map.values())
    return list(in_map.keys())[list_of_values.index(value)] if value in list_of_values else None

def doc_print_dict(d):#define d
    pretty_dict = ''  #take empty string
    for k, v in d.items():#get items for dict
        pretty_dict += f' {k}:{str(v)} |'
    return pretty_dict#return result

# DC fields of worker must be suitable with erlang atoms convention! (lower case first letter)
# Please run ./NerlnetBuild.sh to test any changes of this file!
KEY_DOC_PREFIX = "_doc_"
KEY_MODEL_TYPE = "modelType"
KEY_MODEL_TYPE_DOC = "_doc_modelType"
KEY_LAYER_SIZES_LIST = "layersSizes"
KEY_LAYER_SIZES_DOC = "_doc_layersSizes"
KEY_LAYER_TYPES_LIST = "layerTypesList"
KEY_LAYER_TYPES_DOC = "_doc_LayerTypes"
KEY_LAYERS_FUNCTIONS = "layers_functions"
KEY_LAYERS_FUNCTIONS_ACTIVATION_DOC = "_doc_layers_functions_activation"
KEY_LAYERS_FUNCTIONS_SCALER_DOC = "_doc_layer_functions_scaler"
KEY_LAYERS_FUNCTIONS_POOLING_DOC = "_doc_layer_functions_pooling"
KEY_LAYERS_FUNCTIONS_PROBABILISTIC_DOC = "_doc_layer_functions_probabilistic"
KEY_LOSS_METHOD = "lossMethod"
KEY_LOSS_METHOD_DOC = "_doc_lossMethod"
KEY_EPOCHS = "epochs"
KEY_EPOCHS_DOC = "_doc_epochs"
KEY_LEARNING_RATE = "lr"
KEY_LEARNING_RATE_DOC = "_doc_lr"
KEY_OPTIMIZER_TYPE = "optimizer"
KEY_OPTIMIZER_TYPE_DOC = "_doc_optimizer"
KEY_INFRA_TYPE = "infraType"
KEY_INFRA_TYPE_DOC = "_doc_infraType"
KEY_DISTRIBUTED_SYSTEM_TYPE = "distributedSystemType"
KEY_DISTRIBUTED_SYSTEM_TYPE_DOC = "_doc_distributedSystemType"
KEY_DISTRIBUTED_SYSTEM_TOKEN = "distributedSystemToken"
KEY_DISTRIBUTED_SYSTEM_TOKEN_DOC = "_doc_distributedSystemToken"

VAL_MODEL_TYPE_DOC = f"{doc_print_dict(ModelTypeMapping)}"
VAL_LAYER_SIZES_DOC = "List of postive integers [L0, L1, ..., LN]"
VAL_LAYER_TYPES_DOC = f"{doc_print_dict(LayerTypeMap)}"
VAL_LAYERS_FUNCTIONS_SCALER_DOC = f"{doc_print_dict(ScalingMethodMap)}"
VAL_LAYERS_FUNCTIONS_POOLING_DOC = f"{doc_print_dict(PoolingMethodMap)}"
VAL_LAYERS_FUNCTIONS_PROBABILISTIC_DOC = f"{doc_print_dict(ProbabilisticActivationFunctionMap)}"
VAL_LAYERS_FUNCTIONS_ACTIVATION_DOC = f"{doc_print_dict(ActivationFunctionsMap)}"
VAL_LOSS_METHOD_DOC = f"{doc_print_dict(LossMethodMapping)}"
VAL_EPOCHS_DOC = "Positve Integer"
VAL_LEARNING_RATE_DOC = "Positve float"
VAL_OPTIMIZER_TYPE_DOC = f"{doc_print_dict(OptimizerTypeMapping)}"
VAL_INFRA_TYPE_DOC = f"{doc_print_dict(InfraTypeMapping)}"
VAL_DISTRIBUTED_SYSTEM_TYPE_DOC = f"{doc_print_dict(DistributedSystemTypeMapping)}"
VAL_DISTRIBUTED_SYSTEM_TOKEN_DOC = "Token that associates distributed group of workers and parameter-server"