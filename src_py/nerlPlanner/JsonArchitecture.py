from JsonElements import *
from JsonValidator import *
from collections import OrderedDict
from JsonElementWorker import *

#   from collections import OrderedDict
#values=json.loads(jsontext,object_pairs_hook=OrderedDict)

KEY_NERLNET_SETTINGS = "NerlNetSettings"
KEY_DEVICES = "devices"
KEY_CLIENTS = "clients"
KEY_WORKERS = "workers"
KEY_SOURCES = "sources"
KEY_ROUTERS = "routers"

class JsonArchitecture():
    def __init__(self):
        self.main_dict = OrderedDict()
        self.ports_set = set()
        self.names_set = set()
        self.special_entities_list = [MainServer.NAME, ApiServer.NAME, NerlGUI.NAME]
        self.reserved_names_set = set(self.special_entities_list)
        self.init_dictionary()
        self.entities = []
    
    def init_dictionary(self):
        self.main_dict[KEY_NERLNET_SETTINGS] = ""
        self.main_dict[MainServer.NAME] = ""
        self.main_dict[ApiServer.NAME] = ""
        self.main_dict[NerlGUI.NAME] = ""
        self.main_dict[KEY_ROUTERS] = []
        self.main_dict[KEY_SOURCES] = []
        self.main_dict[KEY_CLIENTS] = []
        self.main_dict[KEY_WORKERS] = []

    def clear(self):
        self.init_dictionary()

    def get_entities(self):
        self.entities += [x for x in [self.reserved_names_set] if self.main_dict[x]]

    def add_nerlnet_settings(self, Frequency, BatchSize):
        key, value = Frequency.get_name_as_tuple()
        self.main_dict[KEY_NERLNET_SETTINGS][key][value]
        key, value = BatchSize.get_name_as_tuple()
        self.main_dict[KEY_NERLNET_SETTINGS][key][value]

    def add_main_server(self, main_server : MainServer):
        self.main_dict[MainServer.NAME] = main_server.get_as_dict()

    def add_api_server(self, api_server : ApiServer):
        self.main_dict[ApiServer.NAME] = api_server.get_as_dict()   

    # nerlgui is optional but requires former attributes to be added first!
    def add_nerlgui_server(self, nerl_gui : NerlGUI):
        self.main_dict[NerlGUI.NAME] = nerl_gui.get_as_dict()  

    def add_client(self, client : Client): 
        '''
        return false if name is being used
        '''     
        client_name = client.get_name()
        if client_name in self.names_set:
            return False
        if client_name in self.reserved_names_set:
            raise "reserved name is being used with a client!"
        else:
            self.main_dict[KEY_CLIENTS].append(client.get_as_dict())
        return True

    def add_router(self, router : Router): 
        '''
        return false if name is being used
        '''      
        router_name = router.get_name()
        if router_name in self.names_set:
            return False
        if router_name in self.reserved_names_set:
            raise "reserved name is being used with a client!"
        else:
            self.main_dict[KEY_ROUTERS].append(router.get_as_dict())
        return True
    
    def add_source(self, source : Source): 
        '''
        return false if name is being used
        '''
        source_name = source.get_name()
        if source_name in self.names_set:
            return False
        if source_name in self.reserved_names_set:
            raise "reserved name is being used with this client!"
        else:
            self.main_dict[KEY_SOURCES].append(source.get_as_dict())
        return True

    def add_worker(self, worker : Worker):
        worker_name = worker.get_name()
        if worker_name in self.names_set:
            return False
        if worker_name in self.reserved_names_set:
            self.main_dict[KEY_WORKERS].append(worker.get_as_dict())
            raise "reserved name is being used with this worker!"
        return True

    def reserved_name(self, name):
        return name in self.reserved_names_set