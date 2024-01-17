
class EntityDB:
    def __init__(self):
        # based on stats.erl
        self.messages_received = 0
        self.messages_sent = 0
        self.messages_dropped = 0
        self.bytes_received = 0
        self.bytes_sent = 0
        self.bad_messages = 0

class RouterDB(EntityDB):
    def __init__(self):
        pass

class WorkerStatsDB(): # WorkerDB is the ML stats - don't confuse!
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

        self.workers_stats_dbs = {} # dictionary of worker_name -> WorkerStatsDB
    
    def add_worker(self, worker_name):
        self.workers_dbs[worker_name] = WorkerStatsDB()
    
    def get_worker(self, worker_name):
        if worker_name not in self.workers_dbs:
            return None
        return self.workers_stats_dbs[worker_name]

class SourceDB(EntityDB):
    def __init__(self):
        self.batches_sent = 0


class NerlComDB():
    def __init__(self):
        self.routers = {}
        self.clients = {}
        self.sources = {}

    def add_router(self, router_name):
        self.routers[router_name] = RouterDB()
    
    def add_client(self, client_name):
        self.clients[client_name] = ClientDB()
    
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