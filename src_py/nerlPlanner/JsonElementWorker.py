from hashlib import sha256
import graphviz as gviz
import pydot
import json
from collections import OrderedDict

from JsonElements import JsonElement
from JsonElements import Epochs
from JsonElements import Arguments
from JsonElementsDefinitions import *
from JsonElementWorkerDefinitions import *

class Optimizer(JsonElement):
    NAME = "optimizer"
    def __init__(self, in_type_str : str, args : str):
        super().__init__(Optimizer.NAME, OPTIMIZER_TYPE)
        self.in_type_str = in_type_str
        self.args = Arguments(args)
        self.optimizer_val = OptimizerTypeMapping[in_type_str] if self.in_type_str in OptimizerTypeMapping else None
    
    def error(self):
        return not self.optimizer_val
    
    def get_val(self):
        return self.optimizer_val

    def get_as_tuple(self):
        return (self.get_name(), self.optimizer_val)
  
    def get_val_str(self):
        return self.in_type_str
        
    def get_args(self) -> str:
        return self.args.get_value()
    
    def __str__(self):
        return self.in_type_str
    
    def get_key_from_value(value):
        return get_key_by_value(OptimizerTypeMapping, value)

class Infra(JsonElement):
    def __init__(self, in_type_str : str):
        super(Infra, self).__init__("infra", INFRA_TYPE)
        self.in_type_str = in_type_str
        self.infra_val = InfraTypeMapping[in_type_str] if self.in_type_str in InfraTypeMapping else None
        
    def error(self):
        return not self.infra_val

    def get_val_str(self):
        return self.in_type_str

    def get_as_tuple(self):
        return (self.get_name(), self.infra_val)
    
    def __str__(self):
        return self.in_type_str
    
    def get_key_from_value(value):
        return get_key_by_value(InfraTypeMapping, value)
    
class DistributedSystemType(JsonElement):
    def __init__(self, in_type_str : str, args: str):
        super(DistributedSystemType, self).__init__("distributedSystemType", DISTRIBUTED_SYSTEM_TYPE)
        self.in_type_str = in_type_str
        self.distributed_system_type_val = DistributedSystemTypeMapping[in_type_str] if self.in_type_str in DistributedSystemTypeMapping else None
        self.args = Arguments(args)

    def error(self):
        return not self.distributed_system_type_val

    def get_val_str(self):
        return self.in_type_str
    
    def get_val(self):
        return self.distributed_system_type_val
    
    def get_args(self) -> str:
         return self.args.get_value()

    def get_as_tuple(self):
        return (self.get_name(), self.distributed_system_type_val)
    
    def __str__(self):
        return get_inv_dict(DistributedSystemTypeMapping)[self.in_type_str]
    
    def get_key_from_value(value):
        return get_key_by_value(DistributedSystemTypeMapping, value)

class DistributedSystemToken(JsonElement):
    def __init__(self, in_token_str : str):
        super(DistributedSystemToken, self).__init__("distributedSystemToken", DISTRIBUTED_SYSTEM_TOKEN_TYPE)
        self.value = in_token_str

    def error(self):
        return not self.value

    def get_val_str(self):
        return self.value

    def get_as_tuple(self):
        return (self.get_name(), self.value)
    
    def __str__(self):
        return self.value
    
class Worker(JsonElement):      
    def __init__(self, name, layers_sizes_list_str : str, model_type_str : str, model_type : int, model_args_str, optimization_type : str,
                 optimizer_args : str,loss_method_str : str, loss_method : int, loss_args : str, learning_rate : str, epochs : str, layer_functions_codes_list_str : str,
                 layer_types_list_str : str, infra : str, distributed_system_type : str, distributed_args : str, distributed_system_token : str):
        super(Worker, self).__init__(name, WORKER_TYPE)
        # Update here when adding new fields to the worker
        self.LayersSizesListStr = layers_sizes_list_str
        self.LayersSizesList = layers_sizes_list_str.split(',')
        self.ModelTypeStr = model_type_str
        self.ModelType = model_type # None
        self.ModelArgs = model_args_str
        self.optimizer = Optimizer(optimization_type, optimizer_args)
        self.LossMethodStr = loss_method_str
        self.LossMethod = loss_method # None
        self.LossArgs = loss_args
        self.LearningRate = float(learning_rate)
        self.LayersFunctionsCodesListStr = layer_functions_codes_list_str
        self.LayersFunctionsCodesList = layer_functions_codes_list_str.split(',') #TODO validate
        self.LayerTypesListStr = layer_types_list_str
        self.LayerTypesList = layer_types_list_str.split(',') #TODO validate
        self.Epochs = Epochs(epochs)
        self.Infra = Infra(infra)
        self.distributedSystemType = DistributedSystemType(distributed_system_type, distributed_args)
        self.distributedSystemToken = DistributedSystemToken(distributed_system_token)

        # validate lists sizes 
        lists_for_length = [self.LayersSizesList, self.LayersFunctionsCodesList, self.LayerTypesList]
        list_of_lengths = [len(x) for x in lists_for_length]
        self.lengths_validation = all([x == list_of_lengths[0] for x in list_of_lengths])

    def generate_graphviz(self):
        self.layers_graph = gviz.Digraph()   
        self.layers_graph.graph_attr['fontname'] = "helvetica"
        self.layers_graph.node_attr['fontname'] = "helvetica"
        self.layers_graph.edge_attr['fontname'] = "helvetica"

        # create nodes
        for curr_layer_idx, curr_layer_size in enumerate(self.LayersSizesList):
            curr_layer_type_num = self.LayerTypesList[curr_layer_idx]
            curr_layer_function_num = self.LayersFunctionsCodesList[curr_layer_idx]
            curr_layer_function_str = ""

            get_layer_type_str = get_key_by_value(LayerTypeMap,curr_layer_type_num)
            layer_type_dict = LayerTypeToFunctionalMap[get_layer_type_str]

            if layer_type_dict:
                curr_layer_function_str = get_key_by_value(layer_type_dict, curr_layer_function_num)

            label = f'Type: {get_layer_type_str} Size: {curr_layer_size} Func: {curr_layer_function_str}'

            self.layers_graph.node(str(curr_layer_idx),label=label, shape='Mrecord',fontsize=str(13), labelfontsize=str(13))
        
        for curr_layer_idx in range(0,len(self.LayersSizesList)-1):
            self.layers_graph.edge(str(curr_layer_idx), str(curr_layer_idx+1))
        
        return self.layers_graph

    def save_graphviz(self,path):
        filename_dot = f"{path}/worker_graph_{self.get_sha()}.dot"
        filename_png = f"{path}/worker_graph_{self.get_sha()}.png"

        layers_graph = self.generate_graphviz()
        layers_graph.save(filename_dot)

        (graph,) = pydot.graph_from_dot_file(filename_dot)
        graph.write_png(filename_png)
        return filename_png

    def copy(self, name):
        # Update here when adding new fields to the worker
        newWorker =  Worker(name, self.LayersSizesListStr, self.ModelTypeStr, self.ModelType , self.ModelArgs , self.Optimizer.get_val_str(), self.Optimizer.get_args(),
                 self.LossMethodStr, self.LossMethod, self.LearningRate, self.Epochs.get_value_str(), self.LayersFunctionsCodesListStr, self.LayerTypesListStr, self.Infra.get_val_str(),
                 self.distributedSystemType.get_val_str(), self.distributedSystemType.get_args(), self.distributedSystemToken.get_val_str())
        return newWorker

    def __str__(self):
        return f"layers sizes: {self.LayersSizesListStr}, model: {self.ModelTypeStr}, using optimizer: {self.optimizer.get_val_str()}, loss method: {self.LossMethodStr}, loss args: {self.LossArgs} ,lr: {self.LearningRate}, epochs: {self.Epochs.get_value_str()}, infra: {self.Infra.get_val_str()}, distributed system type: {self.distributedSystemType.get_val_str()}, distributed system token: {self.distributedSystemToken.get_val_str()}"
    
    def error(self): 
        return not self.input_validation() # + more checks

    def input_validation(self):
        # TODO add more validation: e.g., numbers of keys appears in dictionaries
        return self.lengths_validation and (not self.Epochs.error())
    
    def get_as_dict(self, documentation = True):
        # Update here when adding new fields to the worker
        assert not self.error()
        self.key_val_pairs = [
            (KEY_MODEL_TYPE, self.ModelType),
            (KEY_MODEL_TYPE_DOC, VAL_MODEL_TYPE_DOC),
            (KEY_MODEL_ARGS, self.ModelArgs),
            (KEY_MODEL_ARGS_DOC, VAL_MODEL_ARGS_DOC),
            (KEY_LAYER_SIZES_LIST, self.LayersSizesListStr),
            (KEY_LAYER_SIZES_DOC, VAL_LAYER_SIZES_DOC),
            (KEY_LAYER_TYPES_LIST, self.LayerTypesListStr),
            (KEY_LAYER_TYPES_DOC, VAL_LAYER_TYPES_DOC),
            (KEY_LAYERS_FUNCTIONS, self.LayersFunctionsCodesListStr),
            (KEY_LAYERS_FUNCTIONS_ACTIVATION_DOC, VAL_LAYERS_FUNCTIONS_ACTIVATION_DOC),
            (KEY_LAYERS_FUNCTIONS_POOLING_DOC, VAL_LAYERS_FUNCTIONS_POOLING_DOC),
            (KEY_LAYERS_FUNCTIONS_PROBABILISTIC_DOC, VAL_LAYERS_FUNCTIONS_PROBABILISTIC_DOC),
            (KEY_LAYERS_FUNCTIONS_SCALER_DOC, VAL_LAYERS_FUNCTIONS_SCALER_DOC),
            (KEY_LOSS_METHOD, self.LossMethod),
            (KEY_LOSS_METHOD_DOC, VAL_LOSS_METHOD_DOC),
            (KEY_LOSS_ARGS, self.LossArgs),
            (KEY_LOSS_ARGS_DOC, VAL_LOSS_ARGS_DOC)
            (KEY_LEARNING_RATE, self.LearningRate),
            (KEY_LEARNING_RATE_DOC, VAL_LEARNING_RATE_DOC),
            (KEY_EPOCHS, self.Epochs.get_value_str()),
            (KEY_EPOCHS_DOC, VAL_EPOCHS_DOC),
            (KEY_OPTIMIZER_TYPE, self.optimizer.get_val()),
            (KEY_OPTIMIZER_TYPE_DOC, VAL_OPTIMIZER_TYPE_DOC),
            (KEY_OPTIMIZER_ARGS, self.optimizer.get_args()),
            (KEY_OPTIMIZER_ARGS_DOC, VAL_OPTIMIZER_ARGS_DOC),
            (KEY_INFRA_TYPE, self.Infra.infra_val),
            (KEY_INFRA_TYPE_DOC, VAL_INFRA_TYPE_DOC),
            (KEY_DISTRIBUTED_SYSTEM_TYPE, self.distributedSystemType.get_val()),
            (KEY_DISTRIBUTED_SYSTEM_TYPE_DOC, VAL_DISTRIBUTED_SYSTEM_TYPE_DOC),
            (KEY_DISTRIBUTED_SYSTEM_ARGS, self.distributedSystemType.get_args()),
            (KEY_DISTRIBUTED_SYSTEM_ARGS_DOC, VAL_DISTRIBUTED_SYSTEM_ARGS_DOC),
            (KEY_DISTRIBUTED_SYSTEM_TOKEN, self.distributedSystemToken.get_val_str()),
            (KEY_DISTRIBUTED_SYSTEM_TOKEN_DOC, VAL_DISTRIBUTED_SYSTEM_TOKEN_DOC),
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

    def load_from_dict(worker_dict : dict, name = '', get_params = False):
        # Update here when adding new fields to the worker
        required_keys = [KEY_LAYER_SIZES_LIST, KEY_MODEL_TYPE, KEY_MODEL_ARGS,KEY_OPTIMIZER_TYPE, KEY_OPTIMIZER_ARGS,
                         KEY_LOSS_METHOD, KEY_LOSS_ARGS, KEY_LEARNING_RATE, KEY_EPOCHS, KEY_LAYERS_FUNCTIONS,
                         KEY_LAYER_TYPES_LIST, KEY_EPOCHS, KEY_INFRA_TYPE, KEY_DISTRIBUTED_SYSTEM_TYPE, KEY_DISTRIBUTED_SYSTEM_ARGS, KEY_DISTRIBUTED_SYSTEM_TOKEN]

        all_keys_exist = all([key in worker_dict for key in required_keys])

        if all_keys_exist:
            # Update here when adding new fields to the worker
            LayersSizesList = worker_dict[KEY_LAYER_SIZES_LIST]
            ModelType = int(worker_dict[KEY_MODEL_TYPE])
            ModelTypeStr = get_key_by_value(ModelTypeMapping, worker_dict[KEY_MODEL_TYPE])
            ModelArgs = worker_dict[KEY_MODEL_ARGS]
            OptimizationType = Optimizer.get_key_from_value(worker_dict[KEY_OPTIMIZER_TYPE])
            Optimizer_args = worker_dict[KEY_OPTIMIZER_ARGS]
            LossMethod = int(worker_dict[KEY_LOSS_METHOD])
            LossMethodStr = get_key_by_value(LossMethodMapping, worker_dict[KEY_LOSS_METHOD])
            LossArgs = worker_dict[KEY_LOSS_ARGS]
            LearningRate = float(worker_dict[KEY_LEARNING_RATE])
            ActivationLayersList = worker_dict[KEY_LAYERS_FUNCTIONS]
            LayerTypesList = worker_dict[KEY_LAYER_TYPES_LIST]
            EpochsStr = worker_dict[KEY_EPOCHS]
            InfraType = Infra.get_key_from_value(worker_dict[KEY_INFRA_TYPE])
            DistributedSystemTypeInst = DistributedSystemType.get_key_from_value(worker_dict[KEY_DISTRIBUTED_SYSTEM_TYPE])
            DistributedSystem_args = worker_dict[KEY_DISTRIBUTED_SYSTEM_ARGS]
            DistributedSystemTokenInst = worker_dict[KEY_DISTRIBUTED_SYSTEM_TOKEN]
            
            if get_params:
                # Update here when adding new fields to the worker
                return (LayersSizesList, ModelTypeStr, ModelType, ModelArgs , OptimizationType, Optimizer_args, LossMethodStr, LossMethod, LossArgs,
                        LearningRate, EpochsStr, ActivationLayersList, LayerTypesList, InfraType,
                        DistributedSystemTypeInst, DistributedSystem_args, DistributedSystemTokenInst)

            # Update here when adding new fields to the worker
            return Worker(name, LayersSizesList, ModelTypeStr, ModelType, ModelArgs, OptimizationType, Optimizer_args, LossMethodStr, LossMethod, LossArgs, LearningRate, EpochsStr, ActivationLayersList, LayerTypesList, InfraType,
                 DistributedSystemTypeInst, DistributedSystem_args, DistributedSystemTokenInst)
                    
        return None