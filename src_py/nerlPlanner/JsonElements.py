import re

ARGS_TYPE = 0
VALUE_TYPE = 1
IP_TYPE = 2
PORT_TYPE = 3
POLICY_TYPE = 4
DEVICE_TYPE = 5
ROUTER_TYPE = 6
CLIENT_TYPE = 7
SOURCE_TYPE = 8
WORKER_TYPE = 9
MAIN_SERVER_TYPE = 10
API_SERVER_TYPE = 11
SPECIAL_ENTITY_TYPE = 12
NONE_TYPE = -1


class JsonElement():
    def __init__(self, name = "none", elem_type = NONE_TYPE):
        self.name = name
        self.elem_type = elem_type

    def content_validator(self):
        raise "content validator must be implemented"

    def get_as_dict(self):
        raise "dict form is not implemented"
    
    def get_as_tuple(self):
        raise "tuple form is not implemented"

    def get_str(self):
        raise "string form is not implemented"
    
    # cases that a json element represent incorrect scenarios
    def error(self):
        raise "error is not defined"
    
    def get_name(self):
        return self.name
    
    def get_name_as_tuple(self):
        return ("name", self.name)

    def get_type(self):
        assert(self.elem_type == NONE_TYPE)

    def help_str():
        raise "help is not implemented"

class Arguments(JsonElement):
    def __init__(self, args : str):
        super(Frequency, self).__init__("args", ARGS_TYPE)
        self.args = args

    def error(self):
        return False # error not supported for args

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.args)

# Basic Units 
class Frequency(JsonElement):
    def __init__(self, value):
        super(Frequency, self).__init__("frequency", VALUE_TYPE)
        self.value = int(value) if isinstance(value, str) else value

    def error(self):
        return self.value < 0

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.value)


class BatchSize(JsonElement):
    def __init__(self, value : int):
        super(BatchSize, self).__init__("batchSize", VALUE_TYPE)
        self.value = int(value) if isinstance(value, str) else value
    
    def error(self):
        return self.value < 1
    
    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.value)
    
class Policy(JsonElement):
    def __init__(self, value : int, entity_type):
        super(BatchSize, self).__init__("policy", POLICY_TYPE)
        self.value = value
        self.entity_type = entity_type
    # TODO
    def error(self):
        return not Policy.validate_policy()

    ROUTER_POLICIES = []
    SOURCE_POLICIES = []
    # returns true if policy appears in entity policies list
    def validate_policy(self):
        if self.entity_type == ROUTER_TYPE:
            return self.value in self.ROUTER_POLICIES
        elif self.entity_type == SOURCE_TYPE:
            return self.value in self.SOURCE_POLICIES
        return False

    def get_as_tuple(self):
   #     assert not self.error()
        return (self.get_name() , self.value)

class Ipv4(JsonElement):
    def __init__(self, address : str):
        super(Ipv4, self).__init__("ipv4", IP_TYPE)
        self.address = address

    def error(self):
        return (not Ipv4.validate_ip(self.address))

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.address)

    def validate_ip(address):
        '''
        https://stackoverflow.com/a/3462840/2698310
        '''
        addressList = address.split('.')
        if len(addressList) != 4:
            return False
        for x in addressList:
            if not x.isdigit():
                return False
            i = int(x)
            if i < 0 or i > 255:
                return False
        return True

class Port(JsonElement):
    def __init__(self, value):
        super(Port, self).__init__("port", PORT_TYPE)
        self.value = int(value) if isinstance(value, str) else value

    def error(self):
        return self.value > 65535 or self.value < 1 # 0 is reserved

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.value)

class NerlGUI(JsonElement):
    NAME = "nerlGUI"
    def __init__(self, ip_address : str, port, args : str):
        super(NerlGUI, self).__init__(NerlGUI.NAME, SPECIAL_ENTITY_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.args = Arguments(args)

    def error(self):
        return self.ip.error() or self.port.error
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.ip.get_as_tuple(),
                         self.port.get_as_tuple(),
                         self.args.get_as_tuple()]
        return dict(elements_list)

class ApiServer(JsonElement):
    NAME = "apiServer"
    def __init__(self, ip_address : str, port, args : str):
        super(ApiServer, self).__init__(ApiServer.NAME, API_SERVER_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.args = Arguments(args)

    def error(self):
        return self.ip.error() or self.port.error 
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.ip.get_as_tuple(),
                         self.port.get_as_tuple(), 
                         self.args.get_as_tuple()]
        return dict(elements_list)

class MainServer(JsonElement):
    NAME = "mainServer"
    def __init__(self, ip_address : str, port, args : str):
        super(MainServer, self).__init__(MainServer.NAME, MAIN_SERVER_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.args = Arguments(args)

    def error(self):
        return self.ip.error() or self.port.error 

    def get_as_dict(self):
        assert not self.error()
        elements_list = [ self.ip.get_as_tuple(),
                          self.port.get_as_tuple(),
                          self.args.get_as_tuple()]
        return dict(elements_list)

class Device(JsonElement):
    def __init__(self, ip_address : str, port, name = "none"):
        super(Device, self).__init__(name, DEVICE_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)

    def error(self):
        return self.ip.error() or self.port.error 

    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple()]
        return dict(elements_list)

# TODO
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

class Router(JsonElement):
    def __init__(self, name, ip_address : str, port, policy):
        super(Router, self).__init__(name, ROUTER_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.policy = Policy(policy, super().get_type())
        
    def error(self):
        return self.ip.error() or self.port.error

    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple(),
                         self.policy.get_as_tuple()]
        return dict(elements_list)

class Source(JsonElement):
    def __init__(self,name, ip_address, port, frequency, policy):
        super(Source, self).__init__(name, SOURCE_TYPE)  
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.frequency = Frequency(frequency)
        self.policy = Policy(policy, super().get_type())

    def error(self):
        return self.ip.error() or self.port.error() or self.policy.error() or self.frequency.error()
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple(),
                         self.frequency.get_as_tuple(),
                         self.policy.get_as_tuple()]
        return dict(elements_list)

class Client(JsonElement):
    def __init__(self, name, ip_address, port):
        super(Source, self).__init__(name, CLIENT_TYPE)  
        self.ip = Ipv4(ip_address)
        self.port = Port(port)

    def error(self):
        return self.ip.error() and self.port.error
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple()]
        return dict(elements_list)