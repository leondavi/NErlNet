from JsonElements import *
from collections import OrderedDict
from JsonElementWorker import *

#   from collections import OrderedDict
#values=json.loads(jsontext,object_pairs_hook=OrderedDict)

KEY_NERLNET_SETTINGS = "NerlNetSettings"
KEY_DEVICES = "devices"
KEY_CLIENTS = "clients"
KEY_WORKERS = "workers"
KEY_WORKERS_SHA = "workers_sha"
KEY_SOURCES = "sources"
KEY_ROUTERS = "routers"

class JsonDistributedConfig():
    def __init__(self):
        self.main_dict = OrderedDict()
        self.ports_set = set()
        self.special_entities_list = [MainServer.NAME, ApiServer.NAME]
        self.reserved_names_set = set(self.special_entities_list)
        self.init_dictionary()
        self.default_frequency = None
    
    def init_dictionary(self):
        self.main_dict[KEY_NERLNET_SETTINGS] = None
        self.main_dict[MainServer.NAME] = None
        self.main_dict[ApiServer.NAME] = None
        self.main_dict[KEY_DEVICES] = OrderedDict()
        self.main_dict[KEY_ROUTERS] = OrderedDict()
        self.main_dict[KEY_SOURCES] = OrderedDict()
        self.main_dict[KEY_CLIENTS] = OrderedDict()
        self.main_dict[KEY_WORKERS] = OrderedDict()
        self.main_dict[KEY_WORKERS_SHA] = OrderedDict()

    def clear(self):
        self.init_dictionary()

    def ports_find_conflicts(self):
        pass #TODO

    def ports_conflicts_fix(self):
        pass #TODO

    def main_server_device_validation(self):
        '''
        Main Server must appear in one of devices!
        '''
        for _ , dev in self.main_dict[KEY_DEVICES].items():
            if MainServer.NAME in dev.get_entities_names():
                return True
        return False

    def export_ready(self):
        main_server_exist = self.main_dict[MainServer.NAME] is not None
        api_server_exist = self.main_dict[ApiServer.NAME] is not None
        settings_exist = self.main_dict[KEY_NERLNET_SETTINGS] is not None
        return main_server_exist and api_server_exist and settings_exist

    def get_entities(self):
        special_entities = [x for x in list(self.reserved_names_set) if (x in self.main_dict) and self.main_dict [x]]
        return self.get_clients_names() + self.get_routers_names() + self.get_sources_names() + special_entities

    def add_nerlnet_settings(self, frequency : Frequency, batchsize : BatchSize):
        self.default_frequency = frequency
        settings_dict_content = [frequency.get_as_tuple(), batchsize.get_as_tuple()]
        self.main_dict[KEY_NERLNET_SETTINGS] = OrderedDict(settings_dict_content)

    def get_frequency(self):
        return self.default_frequency # returns Frequency or None

    def add_main_server(self, main_server : MainServer):
        self.main_dict[MainServer.NAME] = main_server

    def add_api_server(self, api_server : ApiServer):
        self.main_dict[ApiServer.NAME] = api_server

    def get_devices_names(self):
        return list(self.main_dict[KEY_DEVICES].keys())
    
    def get_device_by_name(self, name : str) -> Device:
        return self.main_dict[KEY_DEVICES][name] if name in self.main_dict[KEY_DEVICES] else None
    
    def get_devices_ips(self):
        return list( dev.get_ip().get_address() for _ , dev in self.main_dict[KEY_DEVICES].items())
    

    DEVICE_ADD_ISSUE_WITH_IP = -1
    DEVICE_ADD_ISSUE_WITH_NAME = -2
    DEVICE_ADD_SUCCESS = 0
    def add_device(self, device : Device) -> int:
        if device.get_name() in self.get_devices_names():
            return self.DEVICE_ADD_ISSUE_WITH_NAME
        if device.get_ip() in self.get_devices_ips():
            return self.DEVICE_ADD_ISSUE_WITH_IP
        
        self.main_dict[KEY_DEVICES][device.get_name()] = device
        return self.DEVICE_ADD_SUCCESS
    
    def add_entity_to_device(self, device_name : str , entity_name : str):
        '''
        Input device and entity names that exists in json DC database
        '''
        if (device_name in self.main_dict[KEY_DEVICES]) and (entity_name in self.get_entities()):
            for dev_name in self.get_devices_names():
                dev_inst = self.main_dict[KEY_DEVICES][dev_name]
                if entity_name in dev_inst.get_entities_names():
                    return False
            device_inst = self.get_device_by_name(device_name)
            entity_inst = self.get_entity(entity_name)
            device_inst.add_entity(entity_inst)
        else:
            return False
        return True

    def remove_entity_from_device(self, entity_name : str):
        for dev_name in self.get_devices_names():
            dev_inst = self.main_dict[KEY_DEVICES][dev_name]
            if entity_name in dev_inst.get_entities_names():
                if dev_inst.remove_entity(entity_name):
                    return True
        return False


    def add_client(self, client : Client): 
        '''
        return false if name is being used
        '''     
        client_name = client.get_name()
        if client_name in self.get_entities(): # no dups entities!
            return False
        if client_name in self.reserved_names_set:
            raise "reserved name is being used with a client!"
        else:
            self.main_dict[KEY_CLIENTS][client_name] = client
        return True
    
    def remove_client(self, client_name : str):
        if client_name in self.main_dict[KEY_CLIENTS]:
            del(self.main_dict[KEY_CLIENTS][client_name]) 
        self.remove_entity_from_device(client_name)

    
    def get_clients_names(self):
        return list(self.main_dict[KEY_CLIENTS].keys())
    
    def get_owned_workers_by_clients_dict(self) -> list:
        owned_workers_dict = {}
        for client_name , client in self.main_dict[KEY_CLIENTS].items():
            for worker in client.get_workers_names():
                owned_workers_dict[worker] = client_name
        return owned_workers_dict

    def get_entity(self, entity_name : str):
        '''
        Less safe function since it searches all dictionaries
        '''
        for entity_type in [KEY_ROUTERS, KEY_CLIENTS, KEY_SOURCES]:
            if entity_name in self.main_dict[entity_type]:
                return self.main_dict[entity_type][entity_name]
        if entity_name in self.special_entities_list:
            return self.main_dict[entity_name]
        return None

    def get_entity_with_type(self, entity_name : str, entity_type):
        if entity_type in [KEY_ROUTERS, KEY_CLIENTS, KEY_SOURCES]:
            return self.main_dict[entity_type][entity_name] if entity_name in self.main_dict[entity_type] else None
        raise f"bad entity type {entity_type} is not one of KEY_ROUTERS, KEY_CLIENTS, KEY_SOURCE"

    def get_client(self, client_name : str) -> Client: 
        return self.get_entity_with_type(client_name, KEY_CLIENTS)

    def add_router(self, router : Router): 
        '''
        return false if name is being used
        '''      
        router_name = router.get_name()
        if router_name in self.get_entities():
            return False
        if router_name in self.reserved_names_set:
            raise "reserved name is being used with a client!"
        else:
            self.main_dict[KEY_ROUTERS][router_name] = router
        return True
    
    def get_router(self, router_name : str) -> Router:
        return self.get_entity_with_type(router_name, KEY_ROUTERS)
    
    def get_routers_names(self):
        return list(self.main_dict[KEY_ROUTERS].keys())
    
    def add_source(self, source : Source): 
        '''
        return false if name is being used
        '''
        source_name = source.get_name()
        if source_name in self.get_entities():
            return False
        if source_name in self.reserved_names_set:
            raise "reserved name is being used with this client!"
        else:
            self.main_dict[KEY_SOURCES][source_name] = source
        return True
    
    def get_source(self, source_name : str) -> Source:
        return self.get_entity_with_type(source_name, KEY_SOURCES)
    
    def get_sources_names(self):
        return list(self.main_dict[KEY_SOURCES].keys())

    def add_worker(self, worker : Worker):
        worker_name = worker.get_name()
        workers_names_list = list(self.main_dict[KEY_WORKERS].keys())
        if worker_name in self.get_entities():
            return False
        if worker_name in workers_names_list:
            return False
        if worker_name in self.reserved_names_set:
            raise "reserved name is being used with this worker!"
        else: 
            self.main_dict[KEY_WORKERS][worker_name] = worker # Save the woker instance into dictionary
            worker_sha = worker.get_sha() # generate sha of worker
            if worker_sha not in self.main_dict[KEY_WORKERS_SHA]:
                self.main_dict[KEY_WORKERS_SHA][worker_sha] = [worker_name]
            else:
                self.main_dict[KEY_WORKERS_SHA][worker_sha].append(worker_name)
        return True
    
    def get_worker(self, worker_name : str) -> Worker:
        return self.main_dict[KEY_WORKERS][worker_name]
    
    def get_workers_dict(self):
        return self.main_dict[KEY_WORKERS]

    def get_workers_names_list(self):
        return list(self.main_dict[KEY_WORKERS].keys())

    def reserved_name(self, name):
        return name in self.reserved_names_set
    

    EXPORT_DC_JSON_SUCCESS = 0
    EXPORT_DC_JSON_ISSUE_NO_SPECIAL_ENTITIES_OR_SETTINGS = -1
    EXPORT_DC_JSON_ISSUE_MAIN_SERVER_HAS_NO_DEVICE = -2
    def export_dc_json(self, file_path : str):
        if not self.export_ready():
            return self.EXPORT_DC_JSON_ISSUE_NO_SPECIAL_ENTITIES_OR_SETTINGS
        if not self.main_server_device_validation():
            return self.EXPORT_DC_JSON_ISSUE_MAIN_SERVER_HAS_NO_DEVICE
        
        final_dc_dict = OrderedDict()
        final_dc_dict[KEY_NERLNET_SETTINGS] = self.main_dict[KEY_NERLNET_SETTINGS]
        final_dc_dict[MainServer.NAME] = self.main_dict[MainServer.NAME]
        final_dc_dict[ApiServer.NAME] = self.main_dict[ApiServer.NAME]

        final_dc_dict[KEY_DEVICES] = OrderedDict()
        for key, device in self.main_dict[KEY_DEVICES].items():
            final_dc_dict[KEY_DEVICES][key] = device.get_as_dict()
