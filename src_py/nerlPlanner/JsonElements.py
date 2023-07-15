from JsonElementsDefinitions import *
from collections import OrderedDict

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

    def set_name(self, name):
        self.name = name

    def help_str():
        raise "help is not implemented"
    
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

    def error(self):
        return False # error not supported for args

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.args)

# Basic Units 
class Frequency(JsonElement):
    def __init__(self, value):
        super(Frequency, self).__init__("frequency", VALUE_TYPE)
        self.value = float(value) if isinstance(value, str) else value

    def error(self):
        return self.value < 0

    def get_as_tuple(self):
        assert not self.error()
        return (self.get_name() , self.value)
    
    def get_str(self):
        return f'{self.value}'


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

    def get_value(self):
        return self.value

    def __format__(self, __format_spec: str) -> str:
        return f"Port: {self.value}"

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
        return OrderedDict(elements_list)

class ApiServer(JsonElement):
    NAME = "apiServer"
    def __init__(self, ip_address : str, port, args : str):
        super(ApiServer, self).__init__(ApiServer.NAME, API_SERVER_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.args = Arguments(args)

    def error(self):
        return self.ip.error() or self.port.error()
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.ip.get_as_tuple(),
                         self.port.get_as_tuple(), 
                         self.args.get_as_tuple()]
        return OrderedDict(elements_list)

class MainServer(JsonElement):
    NAME = "mainServer"
    def __init__(self, ip_address : str, port, args : str):
        super(MainServer, self).__init__(MainServer.NAME, MAIN_SERVER_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)
        self.args = Arguments(args)

    def error(self):
        return self.ip.error() or self.port.error()

    def get_as_dict(self):
        assert not self.error()
        elements_list = [ self.ip.get_as_tuple(),
                          self.port.get_as_tuple(),
                          self.args.get_as_tuple()]
        return OrderedDict(elements_list)

class Device(JsonElement):
    def __init__(self, ip_address : str, port, name = "none"):
        super(Device, self).__init__(name, DEVICE_TYPE)
        self.ip = Ipv4(ip_address)
        self.port = Port(port)

    def error(self):
        return self.ip.error() or self.port.error()

    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple()]
        return OrderedDict(elements_list)

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
        return OrderedDict(elements_list)

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
        return OrderedDict(elements_list)

class Client(JsonElement):
    def __init__(self, name, port):
        super(Client, self).__init__(name, CLIENT_TYPE)  
        self.port = Port(port)
        self.workers_dict = OrderedDict()

    def __format__(self, __format_spec: str) -> str:
        workers_dict_as_string = ",".join(list(self.workers_dict.keys()))
        numof_workers = len(list(self.workers_dict.keys()))
        return f"name {self.name} {self.port} {numof_workers} workers"

    def get_port(self):
        return self.port
    
    def set_port(self, port):
        self.port = Port(port)
    
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
        return self.ip.error() and self.port.error
    
    def get_as_dict(self):
        assert not self.error()
        elements_list = [self.get_name_as_tuple(), 
                         self.ip.get_as_tuple(),
                         self.port.get_as_tuple(),
                         ('workers', self.workers_dict)]
        return OrderedDict(elements_list)