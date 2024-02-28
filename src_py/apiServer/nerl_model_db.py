from logger import *
import numpy as np

class BatchDB():
    def __init__(self, batch_id, source_name, tensor_data, duration, batch_timestamp):
        self.batch_id = batch_id
        self.source_name = source_name
        self.tensor_data = tensor_data
        self.duration = duration
        self.batch_timestamp = batch_timestamp

    def get_source_name(self):
        return self.source_name

    def get_tensor_data(self):
        return self.tensor_data
class WorkerModelDB():
    def __init__(self, worker_name):
        self.batches_dict = {}
        self.batches_ts_dict = {}
        self.warn_override = False
        self.worker_name = worker_name

    def create_batch(self, batch_id, source_name, tensor_data, duration, batch_timestamp):
        if batch_id in self.batches_dict:
            if not self.warn_override:
                LOG_WARNING(f"Override batches from batch id: {batch_id} in worker {self.worker_name} in source {source_name}.")
                self.warn_override = True
        self.batches_dict[(source_name, batch_id)] = BatchDB(batch_id, source_name, tensor_data, duration, batch_timestamp)
        self.batches_ts_dict[batch_timestamp] = self.batches_dict[(source_name, batch_id)]

    def get_batch(self, source_name, batch_id):
        assert batch_id in self.batches_dict
        return self.batches_dict[(source_name, batch_id)]
    
    def get_total_batches(self):
        assert len(self.batches_dict) == len(self.batches_ts_dict)
        return len(self.batches_dict)
    
    def get_worker_name(self):
        return self.worker_name

    def get_batches_ts_tansor_data_dict(self):
        batches_ts_tansor_data_dict = {}
        for batch_db in self.batches_ts_dict.values():
            batches_ts_tansor_data_dict[batch_db.batch_timestamp] = batch_db.tensor_data
        return batches_ts_tansor_data_dict
class ClientModelDB():
    def __init__(self):
        self.workers_model_db_dict = {}

    def create_worker(self, worker_name):
        self.workers_model_db_dict[worker_name] = WorkerModelDB(worker_name)

    def get_worker(self, worker_name):
        if worker_name not in self.workers_model_db_dict:
            self.create_worker(worker_name)
        return self.workers_model_db_dict[worker_name]
    
    def get_worker_model_db_dict(self):
        return self.workers_model_db_dict

class NerlModelDB():
    def __init__(self, phase: str):
        self.clients_model_db_dict = {} # dictionary of client_name -> ClientModelDB
        self.phase = phase
    
    def create_client(self, client_name):
        self.clients_model_db_dict[client_name] = ClientModelDB()

    def get_client(self, client_name):
        if client_name not in self.clients_model_db_dict:
            self.create_client(client_name)
        return self.clients_model_db_dict[client_name]
    
    def get_client_names(self):
        return list(self.clients_model_db_dict.keys())
    
    def get_workers_model_db_list(self):
        workers_model_db_list = []
        for client_name in self.clients_model_db_dict.keys():
            workers_model_db_list.extend(list(self.clients_model_db_dict[client_name].get_worker_model_db_dict().values()))
        return workers_model_db_list
    
    # this happanes when all data receive 
    # Todo tabel batch id on workers the values will be average duration
    # Todo tabel source name ,batch id, worker name the value will be tesnor data