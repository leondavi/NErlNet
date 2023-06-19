from JsonElements import *
from JsonValidator import *
from collections import OrderedDict

#   from collections import OrderedDict
#values=json.loads(jsontext,object_pairs_hook=OrderedDict)

KEY_NERLNET_SETTINGS = "NerlNetSettings"
KEY_DEVICES = "devices"
KEY_CLIENTS = "clients"
KEY_WORKERS = "workers"
KEY_SOURCES = "sources"
KEY_ROUTERS = "routers"

class JsonGenerator():
    def __init__(self):
        self.main_dict = OrderedDict()
        self.ports_set = set()
        self.names_set = set()
        self.reserved_names_set = set(MainServer.NAME, ApiServer.NAME, NerlGUI.NAME)
    
    def add_nerlnet_settings(self, Frequency, BatchSize):
        self.main_dict[KEY_NERLNET_SETTINGS] = {}
        key, value = Frequency.get_name_as_tuple()
        self.main_dict[KEY_NERLNET_SETTINGS][key][value]
        key, value = BatchSize.get_name_as_tuple()
        self.main_dict[KEY_NERLNET_SETTINGS][key][value]

    def add_main_server(self, MainServer : MainServer):
        if not (KEY_NERLNET_SETTINGS in self.main_dict):
            raise "Wrong order of JsonGenerator calls"
        self.main_dict[MainServer.NAME] = MainServer.get_as_dict()

    def add_api_server(self, ApiServer : ApiServer):
        if not (MainServer.NAME in self.main_dict):
            raise "Wrong order of JsonGenerator add calls"
        self.main_dict[ApiServer.NAME] = ApiServer.get_as_dict()   

    # nerlgui is optional but requires former attributes to be added first!
    def add_nerlgui_server(self, NerlGUI : NerlGUI):
        if not (ApiServer.NAME in self.main_dict):
            raise "Wrong order of JsonGenerator add calls"
        self.main_dict[NerlGUI.NAME] = NerlGUI.get_as_dict()  

    def add_client(self, client : Client): 
        '''
        return false if name is being used
        '''
        if not KEY_CLIENTS in self.main_dict:
            self.main_dict[KEY_CLIENTS] = []
        
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
        if not KEY_ROUTERS in self.main_dict:
            self.main_dict[KEY_ROUTERS] = []
        
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
        if not KEY_SOURCES in self.main_dict:
            self.main_dict[KEY_SOURCES] = []
        
        source_name = source.get_name()
        if source_name in self.names_set:
            return False
        if source_name in self.reserved_names_set:
            raise "reserved name is being used with a client!"
        else:
            self.main_dict[KEY_SOURCES].append(source.get_as_dict())
        return True

    def add_workers(self, worker : Worker):
        pass # TODO

    def reserved_name(self, name):
        return name in self.reserved_names_set