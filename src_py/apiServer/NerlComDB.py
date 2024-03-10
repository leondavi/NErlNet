from networkComponents import MAIN_SERVER_STR , NetworkComponents
from abc import ABC
class EntityComDB(ABC): # Abstract Class
    def __init__(self):
        # based on stats.erl
        self.messages_received = 0
        self.messages_sent = 0
        self.messages_dropped = 0
        self.bytes_received = 0
        self.bytes_sent = 0
        self.bad_messages = 0
    
    def __add__(self, other):
        self.messages_received += other.messages_received
        self.messages_sent += other.messages_sent
        self.messages_dropped += other.messages_dropped
        self.bytes_received += other.bytes_received
        self.bytes_sent += other.bytes_sent
        self.bad_messages += other.bad_messages
        return self

class MainServerComDB(EntityComDB):
    def __init__(self):
        super().__init__()

    def __add__(self, other):
        return super().__add__(other)
    
    def update_stats(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
    
    def get_as_dict(self):
        return {
            "messages_received": self.messages_received,
            "messages_sent": self.messages_sent,
            "messages_dropped": self.messages_dropped,
            "bytes_received": self.bytes_received,
            "bytes_sent": self.bytes_sent,
            "bad_messages": self.bad_messages
        }
        
class RouterComDB(EntityComDB):
    def __init__(self):
        super().__init__()
    
    def __add__(self, other):
        return super().__add__(other)
    
    def update_stats(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        
    def get_as_dict(self):
        return {
            "messages_received": self.messages_received,
            "messages_sent": self.messages_sent,
            "messages_dropped": self.messages_dropped,
            "bytes_received": self.bytes_received,
            "bytes_sent": self.bytes_sent,
            "bad_messages": self.bad_messages
        }

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

    def __add__(self, other):
        self.bytes_received += other.bytes_received
        self.bytes_sent += other.bytes_sent
        self.bad_messages += other.bad_messages
        self.batches_received_train += other.batches_received_train
        self.batches_received_predict += other.batches_received_predict
        self.batches_dropped_train += other.batches_dropped_train
        self.batches_dropped_predict += other.batches_dropped_predict
        self.batches_sent_train += other.batches_sent_train
        self.empty_batches += other.empty_batches
        self.batches_sent_predict += other.batches_sent_predict
        self.average_time_training += other.average_time_training
        self.average_time_prediction += other.average_time_prediction
        self.acc_time_training += other.acc_time_training
        self.acc_time_prediction += other.acc_time_prediction
        self.nan_loss_count += other.nan_loss_count
        return self

    
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

    def update_stats(self, input_dict):
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


class ClientComDB(EntityComDB):
    def __init__(self):
        super().__init__()
        self.batches_received = 0
        self.batches_dropped = 0

        self.workers_stats_dbs = {} # dictionary of worker_name -> WorkerComDB

    def __add__(self, other):
        self.batches_received += other.batches_received
        self.batches_dropped += other.batches_dropped
        for worker_name in other.workers_stats_dbs:
            if worker_name not in self.workers_stats_dbs:
                self.workers_stats_dbs[worker_name] = WorkerComDB()
            self.workers_stats_dbs[worker_name] = self.workers_stats_dbs[worker_name] + other.workers_stats_dbs[worker_name]
        super().__add__(other)
        return self

    def add_worker(self, worker_name):
        self.workers_stats_dbs[worker_name] = WorkerComDB()
    
    def get_worker(self, worker_name):
        if worker_name not in self.workers_stats_dbs:
            return None
        return self.workers_stats_dbs[worker_name]
    
    def update_stats(self, input_dict):
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
    
class SourceComDB(EntityComDB):
    def __init__(self):
        super().__init__()
        self.batches_sent = 0

    def __add__(self, other):  
        self.batches_sent += other.batches_sent
        super().__add__(other)
        return self

    def update_stats(self, input_dict):
        self.messages_received = input_dict["messages_received"]
        self.messages_sent = input_dict["messages_sent"]
        self.messages_dropped = input_dict["messages_dropped"]
        self.bytes_received = input_dict["bytes_received"]
        self.bytes_sent = input_dict["bytes_sent"]
        self.bad_messages = input_dict["bad_messages"]
        self.batches_sent = input_dict["batches_sent"]
        
    def get_as_dict(self):
        return {
            "messages_received": self.messages_received,
            "messages_sent": self.messages_sent,
            "messages_dropped": self.messages_dropped,
            "bytes_received": self.bytes_received,
            "bytes_sent": self.bytes_sent,
            "bad_messages": self.bad_messages,
            "batches_sent": self.batches_sent
        }

class NerlComDB():
    def __init__(self, networks_components: NetworkComponents):
        self.net_comps = networks_components
        self.main_server = MainServerComDB()
        self.routers = {}
        self.clients = {}
        self.sources = {}
        self.workers = {}
        self.build_dicts()

    def __add__(self, other):   
        self.main_server = self.main_server + other.main_server
        for router_name in other.routers:
            if router_name not in self.routers:
                self.routers[router_name] = RouterComDB()
            self.routers[router_name] = self.routers[router_name] + other.routers[router_name]
        for client_name in other.clients:
            if client_name not in self.clients:
                self.clients[client_name] = ClientComDB()
            self.clients[client_name] = self.clients[client_name] + other.clients[client_name]
        for source_name in other.sources:
            if source_name not in self.sources:
                self.sources[source_name] = SourceComDB()
            self.sources[source_name] = self.sources[source_name] + other.sources[source_name]
        for worker_name in other.workers:
            if worker_name not in self.workers:
                self.workers[worker_name] = WorkerComDB()
            self.workers[worker_name] = self.workers[worker_name] + other.workers[worker_name]   
        return self

    def add_router(self, router_name):
        self.routers[router_name] = RouterComDB()
    
    def add_client(self, client_name):
        self.clients[client_name] = ClientComDB()
        
    def add_source(self, source_name):
        self.sources[source_name] = SourceComDB()

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
    
    def get_main_server(self):
        return self.main_server
    
    def get_routers(self):
        return self.routers
    
    def get_clients(self):
        return self.clients
    
    def get_sources(self):
        return self.sources
    
    def get_workers(self):
        for client_name in self.clients:
            for worker_name in self.clients[client_name].workers_stats_dbs:
                self.workers[worker_name] = self.clients[client_name].workers_stats_dbs[worker_name]
        return self.workers

    def build_dicts(self):
        for router_name in self.net_comps.routers:
            self.add_router(router_name)
        for client_name in self.net_comps.clients:
            self.add_client(client_name)
        for source_name in self.net_comps.sources:
            self.add_source(source_name)
        for worker_name in self.net_comps.workers:
            this_worker_client_name = self.net_comps.map_worker_to_client[worker_name]
            self.clients[this_worker_client_name].add_worker(worker_name)
                        
    def update_entities_stats(self, entity_com_dicts: dict):
        for entity_name in entity_com_dicts:
            entity_stats_dict = entity_com_dicts[entity_name]
            if entity_name in self.routers:
                self.routers[entity_name].update_stats(entity_stats_dict)
            elif entity_name in self.clients:
                self.clients[entity_name].update_stats(entity_stats_dict)
            elif entity_name in self.sources:
                self.sources[entity_name].update_stats(entity_stats_dict)
            elif entity_name in self.workers:
                this_worker_client_name = self.net_comps.map_worker_to_client[entity_name]
                self.clients[this_worker_client_name].get_worker(entity_name).update_stats(entity_stats_dict)
            elif entity_name == MAIN_SERVER_STR:
                self.main_server.update_stats(entity_stats_dict)


