

class BatchDB():
    def __init__(self, batch_id, source_name, tensor_data, duration):
        self.batch_id = batch_id
        self.source_name = source_name
        self.tensor_data = tensor_data
        self.duration = duration

class WorkerModelDB():
    def __init__(self):
        self.batches_dict = {}

    def add_batch(self, batch_id, source_name, tensor_data, duration):
        self.batches_dict[batch_id] = BatchDB(batch_id, source_name, tensor_data, duration)

class ClientModelDB():
    def __init__(self):
        self.workers_model_db_dict = {}

    def add_worker(self, worker_name):
        self.workers_model_db_dict[worker_name] = WorkerModelDB()

class NerlModelDB():
    def __init__(self, phase: str):
        self.clients_model_db_dict = {} # dictionary of client_name -> ClientModelDB
        self.phase = phase
    
    def add_client(self, client_name):
        self.clients_model_db_dict[client_name] = ClientModelDB()

    # this happanes when all data receive 
    # Todo tabel batch id on workers the values will be average duration
    # Todo tabel source name ,batch id, worker name the value will be tesnor data