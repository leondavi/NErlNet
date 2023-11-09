import PySimpleGUI as sg
from JsonElements import *
from collections import OrderedDict
from JsonElementWorker import *

import json


#   from collections import OrderedDict
#values=json.loads(jsontext,object_pairs_hook=OrderedDict)

KEY_NERLNET_SETTINGS = "NerlNetSettings"
KEY_FREQUENCY = "frequency"
KEY_BATCH_SIZE = "batchSize"
KEY_DEVICES = "devices"
KEY_CLIENTS = "clients"
KEY_WORKERS = "workers"
KEY_MODEL_SHA = "model-sha"
KEY_SOURCES = "sources"
KEY_ROUTERS = "routers"

NAME_FIELD = "name"
WORKER_MODEL_SHA_FIELD = "model-sha"

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
        self.main_dict[KEY_MODEL_SHA] = OrderedDict()

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
        settings_dict_content = [(KEY_FREQUENCY,frequency), (KEY_BATCH_SIZE, batchsize)]
        self.main_dict[KEY_NERLNET_SETTINGS] = OrderedDict(settings_dict_content)

    def clear_nerlnet_settings(self):
        self.default_frequency = None
        if self.main_dict[KEY_NERLNET_SETTINGS]:
            del(self.main_dict[KEY_NERLNET_SETTINGS][KEY_FREQUENCY]) 
            del(self.main_dict[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE])
    
    def clear_nerlnet_special_entities_settings(self):
         del (self.main_dict[MainServer.NAME])
         del (self.main_dict[ApiServer.NAME])

    def get_frequency(self):
        return self.default_frequency if self.default_frequency else None # returns Frequency or None
    
    def get_batch_size(self):
        batch_size_field_name = get_batch_size_field_name()
        return self.main_dict[KEY_NERLNET_SETTINGS][get_batch_size_field_name()] if batch_size_field_name in self.main_dict[KEY_NERLNET_SETTINGS] else None
        
    def get_main_server(self):
        return self.main_dict[MainServer.NAME]
    
    def get_api_server(self):
        return self.main_dict[ApiServer.NAME]

    def add_main_server(self, main_server : MainServer):
        self.main_dict[MainServer.NAME] = main_server

    def add_api_server(self, api_server : ApiServer):
        self.main_dict[ApiServer.NAME] = api_server

    def get_devices_names(self):
        return list(self.main_dict[KEY_DEVICES].keys())
    
    def get_device_by_name(self, name : str) -> Device:
        return self.main_dict[KEY_DEVICES][name] if name in self.main_dict[KEY_DEVICES] else None

    def get_device_by_entity(self, entity_name : str) -> Device:
        for device in self.main_dict[KEY_DEVICES]:
            device_inst = self.get_device_by_name(device)
            if entity_name in device_inst.get_entities_names():
                return device_inst
        return None

    def get_devices_ips(self):
        return list( dev.get_ip().get_address() for _ , dev in self.main_dict[KEY_DEVICES].items())
    
    def get_devices_entities(self):
        entities_list = []
        for _ , dev in self.main_dict[KEY_DEVICES].items():
            entities_list += dev.get_entities_names()
        return entities_list


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
    
    def remove_device(self, device_name : str):
        # device_inst = self.main_dict[KEY_DEVICES][device_name]
        # device_inst.entities_dict = None
        del(self.main_dict[KEY_DEVICES][device_name]) 

    
    def add_entity_to_device(self, device_name : str , entity_name : str):
        '''
        Input device and entity names that exist in json DC database
        '''
        if (device_name in self.main_dict[KEY_DEVICES]) and (entity_name in self.get_entities()):
            for dev_name in self.get_devices_names(): # check if the entities is used
                dev_inst = self.main_dict[KEY_DEVICES][dev_name]
                if entity_name in dev_inst.get_entities_names():
                    return False
            device_inst = self.get_device_by_name(device_name)
            entity_inst = self.get_entity(entity_name)
            if device_inst.duplicated_ports_validator(entity_inst.get_port()):  # check if the port is used by device 
                if not self.suggest_alternative_port(entity_inst, device_inst):
                    return False # don't switch the port
            device_inst.add_entity(entity_inst)
        else:
            return False
        return True

    def suggest_alternative_port(self, entity_inst, device_inst : Device):
        ch = sg.popup_yes_no("The port of the entity you selected is in use, would you like to change it to a random port?",  title="suggest alternative port")
        if ch == "Yes":
            new_port = device_inst.generate_random_port()
            entity_inst.set_port(new_port) 
            return True
        else:
            return False

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
    
    def remove_router(self, router_name : str):
        self.remove_entity_from_device(router_name)
        if router_name in self.get_routers_names():
            del self.main_dict[KEY_ROUTERS][router_name]

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
    
    def remove_source(self, source_name : str):
        self.remove_entity_from_device(source_name)
        if source_name in self.get_sources_names():
            del self.main_dict[KEY_SOURCES][source_name]
 
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
            if worker_sha not in self.main_dict[KEY_MODEL_SHA]:
                self.main_dict[KEY_MODEL_SHA][worker_sha] = [worker_name]
            else:
                self.main_dict[KEY_MODEL_SHA][worker_sha].append(worker_name)
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
        frequency_tuple = self.main_dict[KEY_NERLNET_SETTINGS][KEY_FREQUENCY].get_as_tuple()
        batch_size_tuple = self.main_dict[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE].get_as_tuple()
        final_dc_dict[KEY_NERLNET_SETTINGS] = OrderedDict([frequency_tuple, batch_size_tuple])
        final_dc_dict[MainServer.NAME] = self.main_dict[MainServer.NAME].get_as_dict()
        final_dc_dict[ApiServer.NAME] = self.main_dict[ApiServer.NAME].get_as_dict()

        final_dc_dict[KEY_DEVICES] = []
        for _ , device in self.main_dict[KEY_DEVICES].items():
            final_dc_dict[KEY_DEVICES].append(device.get_as_dict())

        final_dc_dict[KEY_ROUTERS] = []
        for _ , router in self.main_dict[KEY_ROUTERS].items():
            final_dc_dict[KEY_ROUTERS].append(router.get_as_dict())

        final_dc_dict[KEY_SOURCES] = []
        for _ , source in self.main_dict[KEY_SOURCES].items():
            final_dc_dict[KEY_SOURCES].append(source.get_as_dict())

        final_dc_dict[KEY_CLIENTS] = []
        for _ , client in self.main_dict[KEY_CLIENTS].items():
            final_dc_dict[KEY_CLIENTS].append(client.get_as_dict())

        final_dc_dict[KEY_WORKERS] = []
        final_dc_dict[KEY_MODEL_SHA] = {}

        workers_of_clients_list = self.get_owned_workers_by_clients_dict().keys()
        for worker_name in workers_of_clients_list:
            worker = self.main_dict[KEY_WORKERS][worker_name]
            worker_sha = worker.get_sha()
            worker_model_ptr = OrderedDict([(NAME_FIELD, worker_name),(WORKER_MODEL_SHA_FIELD, worker_sha)])
            final_dc_dict[KEY_WORKERS].append(worker_model_ptr)
            if worker_sha not in final_dc_dict[KEY_MODEL_SHA]:
                final_dc_dict[KEY_MODEL_SHA][worker_sha] = worker.get_as_dict()  

        json_obj = json.dumps(final_dc_dict, indent=4)

        # Writing to sample.json
        with open(file_path, "w") as outfile:
            outfile.write(json_obj)

        return self.EXPORT_DC_JSON_SUCCESS

    IMPORT_DC_JSON_SUCCESS  = (0, '')
    IMPORT_DC_JSON_ISSUE_LOADING_FILE = (-1, 'loading file!')
    IMPORT_DC_JSON_ISSUE_MAINSERVER = (-2, 'importing main server!')
    IMPORT_DC_JSON_ISSUE_APISERVER = (-3, 'importing api server!')
    IMPORT_DC_JSON_ISSUE_MISSING_MODEL = (-4, 'missing mode')
    IMPORT_DC_JSON_ISSUE_CLIENT_WORKER_IS_MISSING = (-5, 'a client cannot import its worker')
    IMPORT_DC_JSON_ISSUE_DEVICE_MISSING_AN_ENTITY = (-6, 'a device cannot import its entity')
    def import_dc_json(self, json_file_path : str) :
        loaded_dc_dict = None
        with open(json_file_path, 'r') as dc_loaded:
            loaded_dc_dict = json.load(dc_loaded, object_pairs_hook=OrderedDict)
        if not loaded_dc_dict:
            return self.IMPORT_DC_JSON_ISSUE_LOAD
        if MainServer.NAME not in loaded_dc_dict:
            return self.IMPORT_DC_JSON_ISSUE_MAINSERVER
        if ApiServer.NAME not in loaded_dc_dict:
            return self.IMPORT_DC_JSON_ISSUE_APISERVER
        #TODO add more checks

        # main server
        port = loaded_dc_dict[MainServer.NAME][get_port_field_name()]
        args = loaded_dc_dict[MainServer.NAME][get_args_field_name()]
        self.add_main_server(MainServer(port, args))

        # api server
        port = loaded_dc_dict[ApiServer.NAME][get_port_field_name()]
        args = loaded_dc_dict[ApiServer.NAME][get_args_field_name()]
        self.add_api_server(ApiServer(port, args))

        # settings
        frequency = Frequency(loaded_dc_dict[KEY_NERLNET_SETTINGS][KEY_FREQUENCY])
        batch_size = BatchSize(loaded_dc_dict[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE])
        self.add_nerlnet_settings(frequency, batch_size)

        # workers
        list_of_workers_mapping_names_to_sha = loaded_dc_dict[KEY_WORKERS]
        dict_of_models_by_sha = loaded_dc_dict[KEY_MODEL_SHA]

        for worker_dict in list_of_workers_mapping_names_to_sha:
            worker_name = worker_dict[NAME_FIELD]
            worker_model_sha = worker_dict[WORKER_MODEL_SHA_FIELD]
            if worker_model_sha not in dict_of_models_by_sha:
                return self.IMPORT_DC_JSON_ISSUE_MISSING_MODEL
            else:
                model_dict = dict_of_models_by_sha[worker_model_sha]
                (new_loaded_worker , _, _, _, _, _, _, _, _, _, _, _) = Worker.load_from_dict(model_dict)
                new_loaded_worker.set_name(worker_name)
                self.add_worker(new_loaded_worker)

        # clients
        list_of_clients_dicts = loaded_dc_dict[KEY_CLIENTS]
        for client_dict in list_of_clients_dicts:
            client_name = client_dict[NAME_FIELD]
            client_port = client_dict[get_port_field_name()]
            client_workers_str = client_dict[get_workers_field_name()]
            new_loaded_client = Client(client_name, client_port)
            # add workers to new loaded client:
            for worker_name in client_workers_str.split(","):
                if worker_name not in self.main_dict[KEY_WORKERS]:
                    return self.IMPORT_DC_JSON_ISSUE_CLIENT_WORKER_IS_MISSING
                else:
                    client_new_worker_sha = self.main_dict[KEY_WORKERS][worker_name].get_sha()
                    new_loaded_client.add_worker(worker_name,client_new_worker_sha)
            self.add_client(new_loaded_client) # add client to main_dict
        
        # routers
        list_of_routers_dicts = loaded_dc_dict[KEY_ROUTERS]
        for router_dict in list_of_routers_dicts:
            router_name = router_dict[NAME_FIELD]
            router_port = router_dict[get_port_field_name()]
            router_policy = router_dict[get_policy_field_name()]
            self.add_router(Router(router_name, router_port, router_policy))

        # sources
        list_of_sources_dicts = loaded_dc_dict[KEY_SOURCES]
        for source_dict in list_of_sources_dicts:
            source_name = source_dict[NAME_FIELD]
            source_port = source_dict[get_port_field_name()]
            source_frequency = source_dict[get_frequency_field_name()]
            source_epochs = source_dict[get_epochs_field_name()]
            source_policy = source_dict[get_policy_field_name()]
            source_type = source_dict[get_source_type_field_name()]
            self.add_source(Source(source_name, source_port, source_frequency, source_policy, source_epochs, source_type))

        #devices
        list_of_devices_dicts = loaded_dc_dict[KEY_DEVICES]
        for device_dict in list_of_devices_dicts:
            device_name = device_dict[NAME_FIELD]
            device_ipv4 = device_dict[get_ipv4_field_name()]
            entities = device_dict[get_entities_field_name()]
            new_device_loaded = Device(device_ipv4, device_name)
            for entity_name in entities.split(','):
                entity_inst = self.get_entity(entity_name)
                if entity_inst is None:
                    return self.IMPORT_DC_JSON_ISSUE_DEVICE_MISSING_AN_ENTITY
                new_device_loaded.add_entity(entity_inst)
            self.add_device(new_device_loaded)

        return self.IMPORT_DC_JSON_SUCCESS
        