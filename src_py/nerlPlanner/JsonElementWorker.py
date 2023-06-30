from JsonElements import JsonElement
from JsonElementsDefinitions import *
from JsonElementWorkerDefinitions import *
import json
import re
from collections import OrderedDict

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
        self.PoolingListStr = ",".join([f"{x}" for x in self.PoolingList])
        self.ScalingListStr = ",".join([f"{x}" for x in self.ScalingList])
        
        self.IntListOfLayersTypes = self.list_representation_conversion_int_elements(self.LayerTypesList)
        self.IntListOfLayersTypesStr = ",".join([str(x) for x in self.IntListOfLayersTypes])
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
        # TODO add more validation: e.g., numbers of keys appears in dictionaries
        layer_sizes_all_positive_integers = len([x for x in self.IntLayersSizesList if x > 0]) == len(self.IntLayersSizesList)
        return self.lengths_validation and layer_sizes_all_positive_integers
    
    def json_list_representation_conversion(self, listStr : str) -> str:
        return str(self.list_representation_conversion(listStr))
    
    def list_representation_conversion(self, listStr : str) -> list:
        return listStr.split(",")
    
    def list_representation_conversion_int_elements(self, listStr : str) -> list:
        pattern = r'^[1-9]*'
        return [int(re.findall(pattern, x)[0]) for x in self.list_representation_conversion(listStr)]
    
    SCALING_LAYER_TYPE_IDX = "1"
    POOLING_LAYER_TYPE_IDX = "4"
    NO_SCALING_TYPE_IDX = 1
    NO_POOLING_TYPE_IDX = 1
    def generate_pooling_and_scaling_lists(self):
        ListOfLayersTypes = self.list_representation_conversion(self.LayerTypesList)
        PoolingList = [x.split("-")[-1] if self.POOLING_LAYER_TYPE_IDX in x else self.NO_POOLING_TYPE_IDX for x in ListOfLayersTypes]
        ScalingList = [x.split("-")[-1] if self.SCALING_LAYER_TYPE_IDX in x else self.NO_SCALING_TYPE_IDX for x in ListOfLayersTypes]
        return PoolingList, ScalingList
    
    def get_as_dict(self, documentation = True):
        
        assert not self.error()
        self.key_val_pairs = [
            (KEY_MODEL_TYPE, self.ModelType),
            (KEY_MODEL_TYPE_DOC, VAL_MODEL_TYPE_DOC),
            (KEY_LAYER_SIZES_LIST, self.LayersSizesList),
            (KEY_LAYER_SIZES_DOC, VAL_LAYER_SIZES_DOC),
            (KEY_LAYER_TYPES_LIST, self.IntListOfLayersTypesStr),
            (KEY_LAYER_TYPES_DOC, VAL_LAYER_TYPES_DOC),
            (KEY_SCALING_METHOD, self.ScalingListStr),
            (KEY_SCALING_METHOD_DOC, VAL_SCALING_METHOD_DOC),
            (KEY_POOLING_LAYER, self.PoolingListStr),
            (KEY_POOLING_LAYER_DOC, VAL_POOLING_METHOD_DOC),
            (KEY_LAYERS_ACTIVATION_FUNCTIONS, self.ActivationLayersList),
            (KEY_LAYERS_ACTIVATION_FUNCTIONS_DOC, VAL_LAYERS_ACTIVATION_FUNCTIONS_DOC),
            (KEY_LOSS_METHOD, self.LossMethod),
            (KEY_LOSS_METHOD_DOC, VAL_LOSS_METHOD_DOC),
            (KEY_LEARNING_RATE, self.LearningRate),
            (KEY_LEARNING_RATE_DOC, VAL_LEARNING_RATE_DOC),
            (KEY_OPTIMIZER_TYPE, self.OptimizationType),
            (KEY_OPTIMIZER_TYPE_DOC, VAL_OPTIMIZER_TYPE_DOC)
        ]
        if not documentation:
            KEY_IDX = 0
            self.key_val_pairs = [x for x in self.key_val_pairs if KEY_DOC_PREFIX not in x[KEY_IDX]] # remove documentation keys
        self.key_val_pairs = self.dict_as_list_of_pairs_fixer(self.key_val_pairs)
        return OrderedDict(self.key_val_pairs)


    def save_as_json(self, out_file : str, documentation = True):
        with open(out_file,"w") as fd_out:
            json.dump(self.get_as_dict(documentation), fd_out, indent=4)