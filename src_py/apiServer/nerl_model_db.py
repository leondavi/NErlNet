from logger import *
import numpy as np

class BatchDB():
    def __init__(self, batch_id, source_name, tensor_data, duration, batch_timestamp):
        self.batch_id = batch_id
        self.source_name = source_name
        self.tensor_data = tensor_data
        self.duration = duration
        self.batch_timestamp = batch_timestamp

class WorkerModelDB():
    def __init__(self):
        self.batches_dict = {}
        self.warn_override = False

    def create_batch(self, batch_id, source_name, tensor_data, duration, batch_timestamp):
        if batch_id in self.batches_dict:
            if not self.warn_override:
                LOG_WARNING(f"Override batches from batch id: {batch_id}")
                self.warn_override = True
        self.batches_dict[batch_id] = BatchDB(batch_id, source_name, tensor_data, duration, batch_timestamp)

    def get_batch(self, batch_id):
        assert batch_id in self.batches_dict
        return self.batches_dict[batch_id]

class ClientModelDB():
    def __init__(self):
        self.workers_model_db_dict = {}

    def create_worker(self, worker_name):
        self.workers_model_db_dict[worker_name] = WorkerModelDB()

    def get_worker(self, worker_name):
        if worker_name not in self.workers_model_db_dict:
            self.create_worker(worker_name)
        return self.workers_model_db_dict[worker_name]

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


    # this happanes when all data receive 
    # Todo tabel batch id on workers the values will be average duration
    # Todo tabel source name ,batch id, worker name the value will be tesnor data