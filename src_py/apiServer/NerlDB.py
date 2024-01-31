   

class BatchTrainDB:
    def __init__(self, worker_name, source_name, batch_id, batch_size, duration = None):
        self.worker_name = worker_name
        self.source_name = source_name
        self.batch_id = batch_id
        self.batch_size = batch_size
        self.loss_value = None
        self.duration = duration

    def get_as_triplet(self):
        return self.worker_name, self.batch_id, self.loss_values
    
    def update_loss_value(self , within_batch_index , value):
        if within_batch_index >= self.batch_size:
            raise Exception("within_batch_index is not as expected")
        self.loss_value = value

class BatchPredictDB:
    def __init__(self, worker_name, source_name, batch_id, batch_size, label_dim = 1, duration = None):
        self.worker_name = worker_name
        self.source_name = source_name
        self.batch_id = batch_id
        self.duration = duration
        self.label_dim = label_dim
        self.source_name = None
        self.labels = [ [ None ] * label_dim ] * batch_size

    def set_label_dim(self, dim):
        self.label_dim = dim

    def get_as_triplet(self):
        return self.worker_name, self.batch_id, self.labels
    
    def add_label(self, sample_index, label):
        if len(label) != self.label_dim:
            raise Exception("label dim is not as expected")
        if sample_index >= self.batch_size:
            raise Exception("sample_index exceeds batch_size")
        self.labels[sample_index] = label
    
    def add_value(self, sample_index, label_index, value):
        if label_index >= self.label_dim:
            raise Exception("label_index is not as expected")
        if sample_index >= self.batch_size:
            raise Exception("sample_index exceeds batch_size")
        self.labels[sample_index][label_index] = value
    

class WorkerDB:
    PHASE_TRAINING = 0
    PHASE_PREDICTION = 1

    def __init__(self, worker_name, batch_size):
        self.worker_name = worker_name
        self.train_dict = {} # dictionary of batch_id -> BatchTrainDB of this worker
        self.predict_dict = {} # dictionary of batch_id -> BatchPredictDB of this worker
        self.batch_size = batch_size

    def add_train_batch(self, batch_id, batch_size, source_name):
        self.train_dict[batch_id] = BatchTrainDB(self.worker_name, source_name, batch_id, batch_size)
    
    def add_predict_batch(self, batch_id, batch_size):
        self.predict_dict[batch_id] = BatchPredictDB(self.worker_name, batch_id, batch_size)

    def get_as_sorted_by_batch_ids(self, phase):
        batches_list = list(range(0, self.batch_size))
        res_list = [None] * self.batch_size
        for key in batches_list:
            if key in self.train_dict:
                if phase == WorkerDB.PHASE_TRAINING:
                    res_list[key] = self.train_dict[key]
                elif phase == WorkerDB.PHASE_PREDICTION:
                    res_list[key] = self.predict_dict[key]
                else:
                    raise Exception("phase is not as expected")
        return res_list

class ClientDB:
    def __init__(self):
        self.worerksDBs = {} # dictionary of worker_name -> WorkerDB
    
    def add_worker(self, worker_name, batch_size):
        self.worerksDBs[worker_name] = WorkerDB(worker_name, batch_size)
    
class NerlDB:
    def __init__(self) -> None:
        self.clientsDBs = {} # dictionary of client_name -> ClientDB
    
    def add_client(self, client_name):
        self.clientsDBs[client_name] = ClientDB()
    
    def get_client(self, client_name):
        if client_name not in self.clientsDBs:
            raise Exception(f"client {client_name} not found in DB")
        return self.clientsDBs[client_name]
    
    def get_total_workers_count(self):
        res = 0
        for client_name, clientDB in self.clientsDBs.items():
            res += len(clientDB.worerksDBs)
        return res

    def get_workers_brain_data(self, phase = None, filter_by_client = [], filter_by_worker = []):
        res = []
        for client_name, clientDB in self.clientsDBs.items():
            if not filter_by_client or (client_name in filter_by_client):
                for worker_name, workerDB in clientDB.worerksDBs.items():
                    if not filter_by_worker or (worker_name in filter_by_worker):
                        res.append(workerDB.get_as_sorted_by_batch_ids(phase))     
        return res   