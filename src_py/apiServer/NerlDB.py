
import pandas as pd
import math
import hashlib

class DataSetDB:
    PHASE_TRAINING = 0
    PHASE_PREDICTION = 1

    def __init__(self, data_set_name, batch_size):
        self.batch_size = batch_size
        self.data_set_name = data_set_name

    def numof_samples_to_batches(self, numof_samples : int):
        return math.floor(numof_samples / self.batch_size)

class DataSetSourceDB(DataSetDB):
    def __init__(self, data_set_name, temp_path, batch_size, source_name, train_numof_batches, predict_numof_batches):
        super().__init__(data_set_name, temp_path, batch_size)
        self.source_name = source_name
        self.batch_iterator_train = 0 # batch iterator allows to follow sent/received batches status
        self.batch_iterator_predict = 0 # batch iterator allows to follow sent/received batches status
        self.batch_stop_idx_train = train_numof_batches # marks batch where source should stop sending batches
        self.batch_stop_idx_predict = predict_numof_batches # marks batch where source should stop sending batches
        
        self.validate_batch_iterators()

    def validate_batch_iterators(self):
        assert self.batch_stop_idx_train >= self.batch_iterator_train
        assert self.batch_stop_idx_predict >= self.batch_iterator_predict
        assert self.batch_iterator_train >= 0
        assert self.batch_iterator_predict >= 0
        return True

    # use the iterator to control multiple phases of source that sends batches
    # E.g., source sends x/n batches for training, then sends y/n batches for prediction
    # then source sends (n-x)/n batches for training, then sends (n-y)/n batches for prediction
    # returns False if iterator is out of bounds
    def increment_batch_iterator(self, phase, val = 1):
        if phase == DataSetDB.PHASE_TRAINING:
            if self.batch_iterator_train + val > self.batch_stop_idx_train:
                return False
            self.batch_iterator_train += val
        elif phase == DataSetDB.PHASE_PREDICTION:
            if self.batch_iterator_predict + val > self.batch_stop_idx_predict:
                return False
            self.batch_iterator_predict += val
        return True
    
    def reset_batch_iterator(self, phase):
        if phase == DataSetDB.PHASE_TRAINING:
            self.batch_iterator_train = 0
        elif phase == DataSetDB.PHASE_PREDICTION:
            self.batch_iterator_predict = 0
        self.validate_batch_iterators()
    
    def get_batch_iterator(self, phase):
        if phase == DataSetDB.PHASE_TRAINING:
            return self.batch_iterator_train
        elif phase == DataSetDB.PHASE_PREDICTION:
            return self.batch_iterator_predict
    
    def set_batch_stop_idx(self, new_value, phase):
        if phase == DataSetDB.PHASE_TRAINING:
            self.batch_stop_idx_train = new_value
        elif phase == DataSetDB.PHASE_PREDICTION:
            self.batch_stop_idx_predict = new_value
        self.validate_batch_iterators()
    
    def get_batch_stop_idx(self, phase):
        if phase == DataSetDB.PHASE_TRAINING:
            return self.batch_stop_idx_train
        elif phase == DataSetDB.PHASE_PREDICTION:
            return self.batch_stop_idx_predict


class DataSetCsvDB(DataSetDB):
    def __init__(self, csv_dataset_path, csv_temp_path, batch_size):
        super().__init__(csv_dataset_path, csv_temp_path, batch_size)
        self.sources_dict = {} # dictionary of source_name -> DataSetSourceDB
        self.csv_dataset_path = csv_dataset_path
        self.sources_dbs_to_offset_indexes_train_hash = {} # dictionary of source_db_name -> offset to sample index
        self.sources_dbs_to_offset_indexes_predict_hash = {} # dictionary of source_db_name -> offset to sample index
        self.csv_temp_path = csv_temp_path # where to save to and where to send from the train and predict csv data
        self.sha_to_name_and_path = {} # dictionary of sha256 hash -> (source_name, csv_dataset_path)

    def get_source_db_name_and_path_by_sha(self, sha):
        return self.sha_to_name_and_path[sha]

    def generate_hash_source_csv_db_sha(self, source_db_name): # will be useful in future if source using more than a single csv file
        sha256_hash = hashlib.sha256()
        with open(f"{self.csv_dataset_path}_{source_db_name}","rb") as f:
            # Read and update hash string value in blocks of 4K
            for byte_block in iter(lambda: f.read(4096),b""):
                sha256_hash.update(byte_block)
            sha_res = sha256_hash.hexdigest()     
            self.sha_to_name_and_path[sha_res] = (source_db_name, self.csv_dataset_path)
            return sha_res

    def calculate_total_possible_numof_batches(self):
        df = pd.df.read_csv(self.csv_dataset_path)
        total_num_of_batches = len(df) / self.batch_size
        return math.floor(total_num_of_batches)

    def add_source_db(self, source_db_name, starting_samples_offset_global_db_train, ending_samples_offset_global_db_train,
                            starting_samples_offset_global_db_predict, ending_samples_offset_global_db_predict):
        # validate input
        assert starting_samples_offset_global_db_train >= 0
        assert ending_samples_offset_global_db_train >= 0
        assert starting_samples_offset_global_db_predict >= 0
        assert ending_samples_offset_global_db_predict >= 0
        assert ending_samples_offset_global_db_train >= starting_samples_offset_global_db_train
        assert ending_samples_offset_global_db_predict >= starting_samples_offset_global_db_predict
        
        assert source_db_name not in self.sources_dict
        assert source_db_name not in self.sources_dbs_to_offset_indexes_train_hash
        assert source_db_name not in self.sources_dbs_to_offset_indexes_predict_hash

        # update hashes
        self.sources_dbs_to_offset_indexes_train_hash[source_db_name] = starting_samples_offset_global_db_train
        self.sources_dbs_to_offset_indexes_predict_hash[source_db_name] = starting_samples_offset_global_db_predict

        # calculate total batches
        total_num_of_batches_train = ending_samples_offset_global_db_train - starting_samples_offset_global_db_train
        total_num_of_batches_predict = ending_samples_offset_global_db_predict - starting_samples_offset_global_db_predict

        new_source_db = DataSetSourceDB(source_db_name, self.csv_temp_path, self.batch_size, source_db_name, total_num_of_batches_train, total_num_of_batches_predict)
        self.sources_dict[source_db_name] = new_source_db

    def get_source_db(self, source_db_name):
        assert source_db_name in self.sources_dict
        return self.sources_dict[source_db_name]

    def get_global_sample_index(self, source_db_name, batch_idx, sample_within_batch_idx, phase):
        assert sample_within_batch_idx < self.batch_size
        if phase == DataSetCsvDB.PHASE_TRAINING:
            return self.sources_dbs_to_starting_indexes_train_hash[source_db_name] + batch_idx * self.batch_size + sample_within_batch_idx
        elif phase == DataSetCsvDB.PHASE_PREDICTION:
            return self.sources_dbs_to_starting_indexes_predict_hash[source_db_name] + batch_idx * self.batch_size + sample_within_batch_idx

    def get_batch_samples_data(self, source_db_name, batches_index_list, phase):
        starting_offset_index = None
        if phase == DataSetCsvDB.PHASE_TRAINING:
            starting_offset_index =  self.sources_dbs_to_starting_indexes_train_hash[source_db_name]
        else:
            starting_offset_index = self.sources_dbs_to_starting_indexes_predict_hash[source_db_name]
        
        df = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index, nrows=self.batch_size * len(batches_index_list))
        return df

        
    def generate_data_for_source(self, source_db_name):
        """
        Generated files for each source:
        1. training csv file:   <source_db_name>_<data_set_name>_training.csv
        2. prediction csv file: <source_db_name>_<data_set_name>_predict.csv

        return
        - filepath_train
        - filepath_predict
        - filename_train
        - filename_predict
        - number_of_samples_train
        - number_of_samples_predict
        """

        assert source_db_name in self.sources_dict
        source_db_inst = self.sources_dict[source_db_name]

        starting_offset_index_train = self.sources_dbs_to_offset_indexes_train_hash[source_db_name]
        starting_offset_index_predict = self.sources_dbs_to_offset_indexes_predict_hash[source_db_name]

        number_of_samples_train = source_db_inst.get_batch_stop_idx(DataSetSourceDB.PHASE_TRAINING) * self.batch_size
        number_of_samples_predict = source_db_inst.get_batch_stop_idx(DataSetSourceDB.PHASE_PREDICTION) * self.batch_size

        df_train = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index_train, nrows=number_of_samples_train)
        df_predict = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index_predict, nrows=number_of_samples_predict)

        filename_train = f"{source_db_name}_{super().data_set_name}_training.csv"
        filepath_train = f"{super().temp_path}/{filename_train}"
        df_train.to_csv(filepath_train , index=False)
       
        filename_predict = f"{source_db_name}_{super().data_set_name}_predict.csv"
        filepath_predict = f"{super().temp_path}/{filename_predict}"
        df_predict.to_csv(filepath_predict , index=False)

        return filepath_train, filepath_predict,\
               filename_train, filename_predict,\
               number_of_samples_train, number_of_samples_predict

        

class BatchTrainDB:
    def __init__(self, worker_name, source_name, batch_id, batch_size, duration = None):
        self.worker_name = worker_name
        self.source_name = source_name
        self.batch_id = batch_id
        self.batch_size = batch_size
        self.loss_values = [ None ] * batch_size
        self.duration = duration

    def get_as_triplet(self):
        return self.worker_name, self.batch_id, self.loss_values
    
    def update_loss_values_list(self , list_of_values):
        if len(list_of_values) != self.batch_size:
            raise Exception("list_of_values is not as expected")
        self.loss_values = list_of_values
    
    def update_loss_value(self , within_batch_index , value):
        if within_batch_index >= self.batch_size:
            raise Exception("within_batch_index is not as expected")
        self.loss_values[within_batch_index] = value

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