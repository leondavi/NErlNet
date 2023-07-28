from hashlib import sha256
import json
import re
from collections import OrderedDict

from JsonElements import JsonElement
from JsonElementsDefinitions import *
from JsonElementWorkerDefinitions import *

class Worker(JsonElement):      
    def __init__(self, name, LayersSizesListStr : str, ModelTypeStr : str, ModelType : int, OptimizationTypeStr : str, OptimizationType : int,
                 LossMethodStr : str, LossMethod : int, LearningRate : str, LayersFunctionsCodesListStr : str, LayerTypesListStr : str):
        super(Worker, self).__init__(name, WORKER_TYPE)
        self.LayersSizesListStr = LayersSizesListStr
        self.LayersSizesList = LayersSizesListStr.split(',')
        self.ModelTypeStr = ModelTypeStr
        self.ModelType = ModelType # None
        self.OptimizationTypeStr = OptimizationTypeStr
        self.OptimizationType = OptimizationType # None
        self.LossMethodStr = LossMethodStr
        self.LossMethod = LossMethod # None
        self.LearningRate = float(LearningRate)
        self.LayersFunctionsCodesListStr = LayersFunctionsCodesListStr
        self.LayersFunctionsCodesList = LayersFunctionsCodesListStr.split(',') #TODO validate
        self.LayerTypesListStr = LayerTypesListStr
        self.LayerTypesList = LayerTypesListStr.split(',') #TODO validate

        # validate lists sizes 
        lists_for_length = [self.LayersSizesList, self.LayersFunctionsCodesList, self.LayerTypesList]
        list_of_lengths = [len(x) for x in lists_for_length]
        self.lengths_validation = all([x == list_of_lengths[0] for x in list_of_lengths])

    def copy(self, name):
        newWorker =  Worker(name, self.LayersSizesList, self.ModelTypeStr, self.ModelType , self.OptimizationTypeStr, self.OptimizationType,
                 self.LossMethodStr, self.LossMethod, self.LearningRate, self.LayersFunctionsCodesList, self.LayerTypesList)
        return newWorker

    def __str__(self):
        return f"LSizes: {self.LayersSizesList}, model {self.ModelTypeStr}, using optimizer {self.OptimizationTypeStr}, loss method: {self.LossMethodStr}, lr: {self.LearningRate}"
    
    def error(self): 
        return not self.input_validation() # + more checks

    def input_validation(self):
        # TODO add more validation: e.g., numbers of keys appears in dictionaries
        layer_sizes_all_positive_integers = len([x for x in self.IntLayersSizesList if x > 0]) == len(self.IntLayersSizesList)
        return self.lengths_validation and layer_sizes_all_positive_integers
    
    def get_as_dict(self, documentation = True):
        assert not self.error()
        self.key_val_pairs = [
            (KEY_MODEL_TYPE, self.ModelType),
            (KEY_MODEL_TYPE_DOC, VAL_MODEL_TYPE_DOC),
            (KEY_LAYER_SIZES_LIST, self.LayersSizesList),
            (KEY_LAYER_SIZES_DOC, VAL_LAYER_SIZES_DOC),
            (KEY_LAYER_TYPES_LIST, self.LayerTypesList),
            (KEY_LAYER_TYPES_DOC, VAL_LAYER_TYPES_DOC),
            (KEY_SCALING_METHOD_DOC, VAL_SCALING_METHOD_DOC),
            (KEY_POOLING_LAYER_DOC, VAL_POOLING_METHOD_DOC),
            (KEY_PROBABILISTIC_LAYER_DOC, VAL_PROBABILISTIC_LAYER_DOC),
            (KEY_LAYERS_FUNCTIONS, self.LayersFunctionsCodesList),
            (KEY_LAYERS_FUNCTIONS_DOC, VAL_LAYERS_ACTIVATION_FUNCTIONS_DOC),
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

    def get_sha(self):
        worker_as_str = f'{self.get_as_dict()}'
        worker_sha = sha256(worker_as_str.encode('utf-8')).hexdigest()
        return worker_sha

    def save_as_json(self, out_file : str, documentation = True):
        with open(out_file,"w") as fd_out:
            json.dump(self.get_as_dict(documentation), fd_out, indent=4)

    def load_from_dict(worker_dict : dict, name = ''):
        required_keys = [KEY_LAYER_SIZES_LIST, KEY_MODEL_TYPE, KEY_OPTIMIZER_TYPE,
                         KEY_LOSS_METHOD, KEY_LEARNING_RATE, KEY_LAYERS_FUNCTIONS,
                         KEY_LAYER_TYPES_LIST]
        
        loaded_worker = None

        all_keys_exist = all([key in worker_dict for key in required_keys])

        if all_keys_exist:
            LayersSizesList = worker_dict[KEY_LAYER_SIZES_LIST]
            ModelType = int(worker_dict[KEY_MODEL_TYPE])
            ModelTypeStr = get_key_by_value(ModelTypeMapping, worker_dict[KEY_MODEL_TYPE])
            OptimizationType = int(worker_dict[KEY_OPTIMIZER_TYPE])
            OptimizationTypeStr = get_key_by_value(OptimizerTypeMapping, worker_dict[KEY_OPTIMIZER_TYPE])
            LossMethod = int(worker_dict[KEY_LOSS_METHOD])
            LossMethodStr = get_key_by_value(LossMethodMapping, worker_dict[KEY_LOSS_METHOD])
            LearningRate = float(worker_dict[KEY_LEARNING_RATE])
            ActivationLayersList = worker_dict[KEY_LAYERS_FUNCTIONS]
            LayerTypesList = worker_dict[KEY_LAYER_TYPES_LIST]
            
            loaded_worker = Worker(name, LayersSizesList, ModelTypeStr, ModelType, OptimizationTypeStr,
                OptimizationType, LossMethodStr, LossMethod, LearningRate, ActivationLayersList, LayerTypesList)
            return loaded_worker, LayersSizesList, ModelTypeStr, ModelType, OptimizationTypeStr,\
                OptimizationType, LossMethodStr, LossMethod, LearningRate, ActivationLayersList, LayerTypesList
        
        return None