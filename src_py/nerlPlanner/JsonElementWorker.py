from JsonElements import JsonElement
from JsonElementsDefinitions import *
import json
from collections import OrderedDict

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

KEY_DOC_PREFIX = "_doc_"
KEY_MODEL_TYPE = "modelType"
KEY_MODEL_TYPE_DOC = "_doc_modelType"
KEY_LAYER_SIZES_LIST = "layersSizes"
KEY_LAYER_SIZES_DOC = "_doc_layersSizes"
KEY_LAYER_TYPES_LIST = "layerTypesList"
KEY_LAYER_TYPES_DOC = "_doc_LayerTypes"
KEY_SCALING_METHOD = "scalingMethod"
KEY_SCALING_METHOD_DOC = "_doc_scalingMethod"
KEY_LAYERS_ACTIVATION_FUNCTIONS = "layersActivationFunctions"
KEY_LAYERS_ACTIVATION_FUNCTIONS_DOC = "_doc_layersActivationFunctions"
KEY_LOSS_METHOD = "lossMethod"
KEY_LOSS_METHOD_DOC = "_doc_lossMethod"
KEY_LEARNING_RATE = "lr"
KEY_LEARNING_RATE_DOC = "_doc_lr"
KEY_OPTIMIZER_TYPE = "optimizer"
KEY_OPTIMIZER_TYPE_DOC = "_doc_optimizer"

def doc_print_dict(d):#define d
    pretty_dict = ''  #take empty string
    for k, v in d.items():#get items for dict
        pretty_dict += f' {k}:{str(v)} |'
    return pretty_dict#return result

VAL_MODEL_TYPE_DOC = f"{doc_print_dict(ModelTypeMapping)}"
VAL_LAYER_SIZES_DOC = "List of postive integers [L0, L1, ..., LN]"
VAL_LAYER_TYPES_DOC = f"{doc_print_dict(LayerTypeMap)}"
VAL_SCALING_METHOD_DOC = f"{doc_print_dict(ScalingMethodMap)}"
VAL_LAYERS_ACTIVATION_FUNCTIONS_DOC = f"{doc_print_dict(ActivationFunctionsMap)}"
VAL_LOSS_METHOD_DOC = f"{doc_print_dict(LossMethodMapping)}"
VAL_LEARNING_RATE_DOC = "Positve float"
VAL_OPTIMIZER_TYPE_DOC = f"{doc_print_dict(OptimizerTypeMapping)}"


class Worker(JsonElement):
    def __init__(self, name, LayersSizesList : str, ModelTypeStr : str, ModelType : int, OptimizationTypeStr : str, OptimizationType : int,
                 LossMethodStr : str, LossMethod : int, LearningRate : str, ActivationLayersList : str, LayerTypesList : str):
        super(Worker, self).__init__(name, WORKER_TYPE)
        self.LayersSizesList = LayersSizesList
        self.ModelTypeStr = ModelTypeStr
        self.ModelType = ModelType # None
        self.OptimizationTypeStr = OptimizationTypeStr
        self.OptimizationType = OptimizationType # None
        self.LossMethodStr = LossMethodStr
        self.LossMethod = LossMethod # None
        self.LearningRate = float(LearningRate)
        self.ActivationLayersList = ActivationLayersList
        self.LayerTypesList = LayerTypesList

        self.PoolingList, self.ScalingList = self.generate_pooling_and_scaling_lists()
        
        self.IntListOfLayersTypes = self.list_representation_conversion_int_elements(self.LayerTypesList)
        self.IntPoolingList = [ int(x) for x in self.PoolingList ]
        self.IntScalingList =  [ int(x) for x in self.ScalingList ]
        self.IntLayersSizesList = self.list_representation_conversion_int_elements(self.LayersSizesList)
        self.IntActivationLayersList = self.list_representation_conversion_int_elements(self.ActivationLayersList)

        # validate lists sizes 
        lists_for_length = [self.IntListOfLayersTypes, self.IntPoolingList , self.IntScalingList , self.IntLayersSizesList, self.IntActivationLayersList ]
        list_of_lengths = [len(x) for x in lists_for_length]
        self.lengths_validation = all([x == list_of_lengths[0] for x in list_of_lengths])

    def __str__(self):
        return f"LSizes: {self.LayersSizesList}, model {self.ModelTypeStr}, using optimizer {self.OptimizationTypeStr}, loss method: {self.LossMethodStr}, lr: {self.LearningRate}"
    
    def error(self): 
        return not self.input_validation() # + more checks

    def input_validation(self):
        layer_sizes_all_positive_integers = len([x for x in self.IntLayersSizesList if x > 0]) == len(self.IntLayersSizesList)
        return self.lengths_validation and layer_sizes_all_positive_integers
    
    def json_list_representation_conversion(self, listStr : str) -> str:
        return str(self.list_representation_conversion(listStr))
    
    def list_representation_conversion(self, listStr : str) -> list:
        return listStr.split(",")
    
    def list_representation_conversion_int_elements(self, listStr : str) -> list:
        return [int(x) for x in self.list_representation_conversion(listStr)]
    
    SCALING_LAYER_TYPE_IDX = "1"
    POOLING_LAYER_TYPE_IDX = "4"
    NO_SCALING_TYPE_IDX = 1
    NO_POOLING_TYPE_IDX = 1
    def generate_pooling_and_scaling_lists(self):
        ListOfLayersTypes = self.list_representation_conversion(self.LayerTypesList)
        PoolingList = [x.split("-")[-1] if self.POOLING_LAYER_TYPE_IDX in x else self.NO_POOLING_TYPE_IDX for x in ListOfLayersTypes]
        ScalingList = [x.split("-")[-1] if self.SCALING_LAYER_TYPE_IDX in x else self.NO_SCALING_TYPE_IDX for x in ListOfLayersTypes]
        return PoolingList, ScalingList
    
    def get_as_dict(self):
        #TODO
        assert not self.error()

        self.key_val_pairs = [
            (KEY_MODEL_TYPE, self.ModelType),
            (KEY_MODEL_TYPE_DOC, )
        ]
        self.worker_dict = OrderedDict()
        self.worker_dict = {
            
        }
        KEY_DOC_PREFIX = "_doc_"
        KEY_MODEL_TYPE = "modelType"
        KEY_MODEL_TYPE_DOC = "_doc_modelType"
        KEY_LAYER_SIZES_LIST = "layersSizes"
        KEY_LAYER_SIZES_DOC = "_doc_layersSizes"
        KEY_LAYER_TYPES_LIST = "layerTypesList"
        KEY_LAYER_TYPES_DOC = "_doc_LayerTypes"
        KEY_SCALING_METHOD = "scalingMethod"
        KEY_SCALING_METHOD_DOC = "_doc_scalingMethod"
        KEY_LAYERS_ACTIVATION_FUNCTIONS = "layersActivationFunctions"
        KEY_LAYERS_ACTIVATION_FUNCTIONS_DOC = "_doc_layersActivationFunctions"
        KEY_LOSS_METHOD = "lossMethod"
        KEY_LOSS_METHOD_DOC = "_doc_lossMethod"
        KEY_LEARNING_RATE = "lr"
        KEY_LEARNING_RATE_DOC = "_doc_lr"
        KEY_OPTIMIZER_TYPE = "optimizer"
        KEY_OPTIMIZER_TYPE_DOC = "_doc_optimizer"

    def save_as_json(self, out_file : str):
        json.dump(self.out_file)