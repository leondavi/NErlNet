from networkComponents import NetworkComponents

class EntityDB:
    def __init__(self):
        # based on stats.erl
        self.messages_received = 0
        self.messages_sent = 0
        self.messages_dropped = 0
        self.bytes_received = 0
        self.bytes_sent = 0
        self.bad_messages = 0

class MainServerDB(EntityDB):
    def __init__(self):
        pass
    
    def read_from_dict(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        
class RouterDB(EntityDB):
    def __init__(self):
        pass
    
    def read_from_dict(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]

class WorkerComDB(): # WorkerDB is the ML stats (train, predict) - don't confuse!
    # based on stats erl
    def __init__(self):
        self.bytes_received = 0
        self.bytes_sent = 0
        self.bad_messages = 0
        self.batches_received_train = 0
        self.batches_received_predict = 0
        self.batches_dropped_train = 0
        self.batches_dropped_predict = 0
        self.batches_sent_train = 0
        self.empty_batches = 0
        self.batches_sent_predict = 0
        self.average_time_training = 0
        self.average_time_prediction = 0
        self.acc_time_training = 0
        self.acc_time_prediction = 0
        self.nan_loss_count = 0
    
    def get_as_dict(self):
        return {
            "bytes_received": self.bytes_received,
            "bytes_sent": self.bytes_sent,
            "bad_messages": self.bad_messages,
            "batches_received_train": self.batches_received_train,
            "batches_received_predict": self.batches_received_predict,
            "batches_dropped_train": self.batches_dropped_train,
            "batches_dropped_predict": self.batches_dropped_predict,
            "batches_sent_train": self.batches_sent_train,
            "empty_batches": self.empty_batches,
            "batches_sent_predict": self.batches_sent_predict,
            "average_time_training": self.average_time_training,
            "average_time_prediction": self.average_time_prediction,
            "acc_time_training": self.acc_time_training,
            "acc_time_prediction": self.acc_time_prediction,
            "nan_loss_count": self.nan_loss_count
        }

    def read_from_dict(self, input_dict):
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        self.batches_received_train = input_dict["batches_received_train"]
        self.batches_received_predict = input_dict["batches_received_predict"]
        self.batches_dropped_train = input_dict["batches_dropped_train"]
        self.batches_dropped_predict = input_dict["batches_dropped_predict"]
        self.batches_sent_train = input_dict["batches_sent_train"]
        self.empty_batches = input_dict["empty_batches"]
        self.batches_sent_predict = input_dict["batches_sent_predict"]
        self.average_time_training = input_dict["average_time_training"]
        self.average_time_prediction = input_dict["average_time_prediction"]
        self.acc_time_training = input_dict["acc_time_training"]
        self.acc_time_prediction = input_dict["acc_time_prediction"]
        self.nan_loss_count = input_dict["nan_loss_count"]


class ClientDB(EntityDB):
    def __init__(self):
        self.batches_received = 0
        self.batches_dropped = 0

        self.workers_stats_dbs = {} # dictionary of worker_name -> WorkerComDB
    
    def add_worker(self, worker_name):
        self.workers_dbs[worker_name] = WorkerComDB()
    
    def get_worker(self, worker_name):
        if worker_name not in self.workers_dbs:
            return None
        return self.workers_stats_dbs[worker_name]
    
    def read_from_dict(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        self.batches_received = input_dict["batches_received"]
        self.batches_dropped = input_dict["batches_dropped"]
        self.batches_sent = input_dict["batches_sent"]
        # self.workers_stats_dbs = input_dict["workers_stats_dbs"]   # Check if the is nessesary

    def get_as_dict(self):
        return {
            "messages_received": self.messages_received,
            "messages_sent": self.messages_sent,
            "messages_dropped": self.messages_dropped,
            "bytes_received": self.bytes_received,
            "bytes_sent": self.bytes_sent,
            "bad_messages": self.bad_messages,
            "batches_received": self.batches_received,
            "batches_dropped": self.batches_dropped,
            "batches_sent": self.batches_sent,
           # "workers_stats_dbs": self.workers_stats_dbs
        }
    
class SourceDB(EntityDB):
    def __init__(self):
        self.batches_sent = 0

    def read_from_dict(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        self.batches_sent = input_dict["batches_sent"]

class NerlComDB():
    def __init__(self, networks_components: NetworkComponents):
        self.net_comps = networks_components
        self.main_server = MainServerDB()
        self.routers = {}
        self.clients = {}
        self.sources = {}
        self.workers = {}
        self.build_dicts()

    def add_router(self, router_name):
        self.routers[router_name] = RouterDB()
    
    def add_client(self, client_name):
        self.clients[client_name] = ClientDB()
    
    def add_worker(self, worker_name):
        self.workers[worker_name] = WorkerComDB()
    
    def add_source(self, source_name):
        self.sources[source_name] = SourceDB()

    def get_router(self, router_name):
        if router_name not in self.routers:
            return None
        return self.routers[router_name]
    
    def get_client(self, client_name):
        if client_name not in self.clients:
            return None
        return self.clients[client_name]
    
    def get_source(self, source_name):
        if source_name not in self.sources:
            return None
        return self.sources[source_name]
    
    def build_dicts(self):
        for router_name in self.net_comps.routers:
            self.add_router(router_name)
        for client_name in self.net_comps.clients:
            self.add_client(client_name)
        for source_name in self.net_comps.sources:
            self.add_source(source_name)
        for worker_name in self.net_comps.workers:
            self.add_worker(worker_name)
            
    def update_entities_stats(self, entity_com_dicts: dict):
        for entity_name in entity_com_dicts:
            entity_stats_dict = entity_com_dicts[entity_name]
            if entity_name in self.routers:
                self.routers[entity_name].read_from_dict(entity_stats_dict)
            elif entity_name in self.clients:
                self.clients[entity_name].read_from_dict(entity_stats_dict)
                #print("Client stats for check: ", self.clients[entity_name].get_as_dict())
            elif entity_name in self.sources:
                self.sources[entity_name].read_from_dict(entity_stats_dict)
            elif entity_name in self.workers:
                self.workers[entity_name].read_from_dict(entity_stats_dict)
            elif entity_name == "mainServer":
                self.main_server.read_from_dict(entity_stats_dict)
                