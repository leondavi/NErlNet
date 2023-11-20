from JsonElementsDefinitions import *
from JsonDistributedConfigDefs import *
from collections import OrderedDict
import random

FIELD_NAME = "name"

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
        assert(self.elem_type != NONE_TYPE)
        return self.elem_type

    def set_name(self, name):
        self.name = name

    def help_str(self):
        raise "help is not implemented"
    
    def communication_elem(self):
        return False
    
    def dict_as_list_of_pairs_fixer(self, input_list : list ) -> list:
        new_list = []
        for key, value in input_list:
            new_tuple = (f'{key}',f'{value}')
            new_list.append(new_tuple)
        return new_list
        

class Arguments(JsonElement):
    def __init__(self, args : str):
        super(Arguments, self).__init__("args", ARGS_TYPE)
        self.args = args

    def get_value(self) -> str:
        return self.args

    def error(self):
        return False # error not supported for args

    def get_as_tuple(self):
        assert not self.error()
        args_str = self.args if self.args else ""
        return (self.get_name() , args_str)

class Epochs(JsonElement):
    '''
    Definition: epoch is one complete pass through the training data
    '''
    def __init__(self, value):
        super(Epochs, self).__init__("epochs", VALUE_TYPE)
        self.value = int(value) if isinstance(value,str) else value

    def get_value_str(self):
        return str(self.value)

    def get_value(self):
        return self.value

    def error(self):
        return self.value <= 0

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name(), str(self.value))

    def get_str(self):
        return f'{self}'

    def __format__(self, __format_spec: str) -> str:
        return f"Epochs: {self.value}" 

# Basic Units 
class Frequency(JsonElement):
    def __init__(self, value):
        super(Frequency, self).__init__("frequency", VALUE_TYPE)
        self.value = int(value) if isinstance(value, str) else value

    def error(self):
        return self.value < 0

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , str(self.value))

    def get_value_str(self):
        return f'{self.value}'

    def get_value(self):
        return self.value


class BatchSize(JsonElement):
    def __init__(self, value : int):
        super(BatchSize, self).__init__("batchSize", VALUE_TYPE)
        self.value = int(value) if isinstance(value, str) else value
        self.int_valid = float(value).is_integer()
    
    def error(self):
        error_conditions = (self.value < 1) and (not self.int_valid)
        return error_conditions
    
    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , str(self.value))
    
    def get_value_str(self):
        return f'{self.value}'

class Policy(JsonElement):
    def __init__(self, value : int, entity_type):
        super(Policy, self).__init__("policy", POLICY_TYPE)
        self.value = value
        self.entity_type = entity_type
    # TODO
    def error(self):
        return not self.validate_policy()

    def get_policy_name(self):
        if self.entity_type == ROUTER_TYPE:
            return get_inv_dict(RouterPolicyDict)[self.value]
        elif self.entity_type == SOURCE_TYPE:
            return get_inv_dict(SourcePolicyDict)[self.value]

    # returns true if policy appears in entity policies list
    def validate_policy(self):
        if self.entity_type == ROUTER_TYPE:
            return self.value in get_inv_dict(RouterPolicyDict)
        elif self.entity_type == SOURCE_TYPE:
            return self.value in get_inv_dict(SourcePolicyDict)
        return False

    def get_as_tuple(self):
   #     assert not self.error()
        return (self.get_name() , str(self.value))

class Ipv4(JsonElement):
    def __init__(self, address : str):
        super(Ipv4, self).__init__("ipv4", IP_TYPE)
        self.address = address

    def get_address(self):
        return self.address

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

    def get_value(self):
        return self.value

    def get_value_str(self):
        return str(self.value)

    def __format__(self, __format_spec: str) -> str:
        return f"Port: {self.value}"

    def error(self):
        return self.value > 65535 or self.value < 1 # 0 is reserved

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , str(self.value))

class ApiServer(JsonElement):
    NAME = "apiServer"
    def __init__(self, port, args : str):
        super(ApiServer, self).__init__(ApiServer.NAME, API_SERVER_TYPE)
        self.port = Port(port)
        self.args = Arguments(args)

    def __format__(self, __format_spec: str) -> str:
        return f"{self.port}"
    
    def get_port(self):
        return self.port

    def set_port(self, new_port : Port):
        self.port = new_port

    def get_args(self) -> str:
        return self.args

    def communication_elem(self):
        return True

    def error(self):
        return self.port.error()
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.port.get_as_tuple(), 
                         self.args.get_as_tuple()]
        return OrderedDict(elements_list)

class MainServer(JsonElement):
    NAME = "mainServer"
    def __init__(self, port, args : str):
        super(MainServer, self).__init__(MainServer.NAME, MAIN_SERVER_TYPE)
        self.port = Port(port)
        self.args = Arguments(args)

    def __format__(self, __format_spec: str) -> str:
        return f"{self.port}"
    
    def get_port(self):
        return self.port
    
    def set_port(self, new_port : Port):
        self.port = new_port

    def get_args(self):
        return self.args

    def communication_elem(self):
        return True

    def error(self):
        return self.port.error()

    def get_as_dict(self):
        assert not self.error()
        elements_list = [ self.port.get_as_tuple(),
                          self.args.get_as_tuple()]
        return OrderedDict(elements_list)

class Device(JsonElement):
    def __init__(self, ip_address : str, name = "none"):
        super(Device, self).__init__(name, DEVICE_TYPE)
        self.ip = Ipv4(ip_address)
        self.entities_dict = OrderedDict()

    def __format__(self, __format_spec: str) -> str:
        return f"Device: {self.name}"
    
    def get_entities_names(self):
        return list(self.entities_dict.keys())

    def duplicated_ports_validator(self, new_entity_port : Port):
        '''
        Checks if there is at least a single occurance of duplicated ports. 
        If there is returns True = duplicate
        '''
        ports_set = set()
        ports_set.add(new_entity_port.get_value())
        for _ , entity in self.entities_dict.items():
            port_val = entity.get_port().get_value()
            if port_val in ports_set:
                return True
            ports_set.add(port_val)
        return False

    def get_ip(self):
        return self.ip

    ADD_ENTITY_SUCCESS = 0
    ADD_ENTITY_ISSUE_DUPLICATED_PORT = -1
    ADD_ENTITY_ISSUE_NAME_EXISTS = -2
    def add_entity(self, entity : JsonElement) -> bool:
        if (entity.get_name() not in self.entities_dict) and entity.communication_elem(): 
            self.entities_dict[entity.get_name()] = entity
            return True
        return False # TODO implement issues hadling
    
    def generate_random_port(self):
        return Port(random.sample(range(49152,65535),1)[0]) # safe range of ports to generate a new port

    def remove_entity(self, entity_name : str) -> bool:
        if entity_name in self.entities_dict:
            del(self.entities_dict[entity_name])
            return True
        return False

    def error(self):
        return self.ip.error()

    def get_as_dict(self):
        assert not self.error()
        entities_str = ",".join(self.get_entities_names())
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         (ENTITIES_FIELD, entities_str)]
        return OrderedDict(elements_list)

class Router(JsonElement):
    def __init__(self, name, port, policy):
        super(Router, self).__init__(name, ROUTER_TYPE)
        self.port = Port(port)
        self.policy = Policy(policy, super().get_type())

    def communication_elem(self):
        return True

    def get_port(self):
        return self.port
    
    def set_port(self, new_port : Port):
        self.port = new_port

    def get_policy(self):
        return self.policy

    def error(self):
        return self.port.error and self.policy.error()

    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.port.get_as_tuple(),
                         self.policy.get_as_tuple()]
        return OrderedDict(elements_list)

class Source(JsonElement):
    def __init__(self,name, port, frequency, policy, epochs, source_type):
        super(Source, self).__init__(name, SOURCE_TYPE)  
        self.port = Port(port)
        self.frequency = Frequency(frequency)
        self.policy = Policy(policy, super().get_type())
        self.epochs = Epochs(epochs)
        self.source_type = source_type

    def communication_elem(self):
        return True

    def source_type_error(self):
        return True if self.source_type not in get_inv_dict(SourceTypeDict) else False

    def get_port(self):
        return self.port

    def set_port(self, new_port : Port):
        self.port = new_port

    def get_frequency(self):
        return self.frequency

    def get_epochs(self):
        return self.epochs

    def get_policy(self):
        return self.policy

    def get_source_type(self):
        return get_inv_dict(SourceTypeDict)[self.source_type]

    def error(self):
        return self.port.error() or self.policy.error() or self.frequency.error() or self.epochs.error() or self.source_type_error()

    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.port.get_as_tuple(),
                         self.frequency.get_as_tuple(),
                         self.policy.get_as_tuple(),
                         self.epochs.get_as_tuple(),
                         (KEY_SOURCES, self.source_type)]
        return OrderedDict(elements_list)

class Client(JsonElement):
    def __init__(self, name, port):
        super(Client, self).__init__(name, CLIENT_TYPE)  
        self.port = Port(port)
        self.workers_dict = OrderedDict()

    def communication_elem(self):
        return True

    def __format__(self, __format_spec: str) -> str:
        # workers_dict_as_string = ",".join(list(self.workers_dict.keys()))
        numof_workers = len(list(self.workers_dict.keys()))
        return f"name {self.name} {self.port} workers: {numof_workers}"

    def get_port(self):
        return self.port

    def set_port(self, new_port : Port):
        self.port = new_port

    def get_workers_names(self):
        return list(self.workers_dict.keys())

    def add_worker(self, worker_name, worker_sha):
        if worker_name not in self.workers_dict:
            self.workers_dict[worker_name] = worker_sha
            return True
        return False

    def remove_worker(self, worker_name):
        if worker_name in self.workers_dict:
            self.workers_dict.pop(worker_name)

    def error(self):
        return self.port.error()
    
    def get_as_dict(self):
        assert not self.error()
        workers_list_str = ",".join(self.get_workers_names())
        elements_list = [self.get_name_as_tuple(), 
                         self.port.get_as_tuple(),
                         (WORKERS_FIELD, workers_list_str)]
        return OrderedDict(elements_list)
    

class GetFields():

    def __init__(self):
        pass

    def get_name_field_name():
        return FIELD_NAME

    def get_main_server_field_name():
        return MainServer.NAME

    def get_api_server_field_name():
        return ApiServer.NAME

    def get_frequency_field_name():
        return Frequency(0).get_name()

    def get_batch_size_field_name():
        return BatchSize(0).get_name()

    def get_port_field_name():
        return Port(0).get_name()

    def get_args_field_name():
        return Arguments('').get_name()

    def get_ipv4_field_name():
        return Ipv4('0.0.0.0').get_name()

    def get_epochs_field_name():
        return Epochs(0).get_name()

    def get_entities_field_name():
        return ENTITIES_FIELD

    def get_workers_field_name():
        return KEY_WORKERS

    def get_source_type_field_name():
        return TYPE_FIELD

    def get_policy_field_name():
        return Policy(0,0).get_name()
    
    def get_clients_field_name():
        return KEY_CLIENTS
    
    def get_routers_field_name():
        return KEY_ROUTERS
    
    def get_sources_field_name():
        return KEY_SOURCES