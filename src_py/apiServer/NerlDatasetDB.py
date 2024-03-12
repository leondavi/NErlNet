
from abc import ABC, abstractmethod
import pandas as pd
import math
import hashlib


class DataSetDB(ABC): # Abstract Class
    PHASE_TRAINING = 0
    PHASE_PREDICTION = 1

    def __init__(self, data_set_name, batch_size):
        self.batch_size = batch_size
        self.data_set_name = data_set_name

    def numof_samples_to_batches(self, numof_samples : int):
        return math.floor(numof_samples / self.batch_size)
# example csv_db = DatasetCsvDB() 
# csv_db.sources_to_sources_db_dict['s1'] = DataSetSourceDB() from init
# csv_db.sources_to_sources_db_dict['s1'].get_phase()
class DataSetSourceDB(DataSetDB):
    def __init__(self, data_set_name, batch_size, numof_batches, phase):
        super().__init__(data_set_name, batch_size)
        self.source_name = None # ties this sourceDB to a specific source from DC
        self.batch_iterator = 0 # batch iterator allows to follow sent/received batches status
        self.batch_stop_index = numof_batches # marks batch where source should stop sending batches
        self.batch_stop_original = numof_batches
        self.phase = phase

        self.validate_batch_iterators()

    def get_phase(self):
        return self.phase

    def set_source(self, source_name):
        self.source_name = source_name
    
    def get_source(self):
        return self.source_name

    def validate_batch_iterators(self):
        assert self.batch_stop_index >= self.batch_iterator
        assert self.batch_iterator >= 0
        return True

    # use the iterator to control multiple phases of source that sends batches
    # E.g., source sends x/n batches for training, then sends y/n batches for prediction
    # then source sends (n-x)/n batches for training, then sends (n-y)/n batches for prediction
    # returns False if iterator is out of bounds
    def increment_batch_iterator(self, val = 1):
        if self.batch_iterator + val > self.batch_stop_idx:
            return False
        self.batch_iterator += val
        return True
    
    def reset_batch_iterator(self, phase):
        self.batch_iterator = 0
        self.validate_batch_iterators()
    
    def get_batch_iterator(self):
        return self.batch_iterator
    
    def set_batch_stop_idx(self, new_value, phase):
        self.batch_stop_idx = new_value
        self.validate_batch_iterators()
    
    # def set_batch_stop_original_value(self, new_value, phase):
    #     if phase == DataSetDB.PHASE_TRAINING:
    #         self.batch_stop_original_value = new_value
    #     elif phase == DataSetDB.PHASE_PREDICTION:
    #         self.batch_stop_original_value = new_value
    #     self.validate_batch_iterators()
    
    # def get_batch_stop_idx(self, phase):
    #     if phase == DataSetDB.PHASE_TRAINING:
    #         return self.batch_stop_idx_train
    #     elif phase == DataSetDB.PHASE_PREDICTION:
    #         return self.batch_stop_idx_predict
        
    # def get_batch_stop_original_value(self, phase):
    #     if phase == DataSetDB.PHASE_TRAINING:
    #         return self.batch_stop_original_value
    #     elif phase == DataSetDB.PHASE_PREDICTION:
    #         return self.batch_stop_original_value


class DataSetCsvDB(DataSetDB):
    def __init__(self, csv_dataset_path, csv_temp_path, batch_size):
        super().__init__(csv_dataset_path, batch_size)
        
        self.sources_to_sources_db_dict = {} # dictionary of source_name -> DataSetSourceDB Instance
        self.source_db_name_to_source_db_inst = {} # dictionary of source_db_name -> DataSetSourceDB Instance
        self.sources_dbs_name_to_offset_indexes_hash = {} # dictionary of source_db_name -> offset to sample index
        
        self.csv_dataset_path = csv_dataset_path
        self.csv_temp_path = csv_temp_path # where to save to and where to send from the train and predict csv data
        self.sha_to_name_and_path = {} # dictionary of sha256 hash -> (source_name, csv_dataset_path)
        self.list_dbs_to_send = []

    def add_source_db(self, source_name, source_db_name, target_workers_names_list, starting_offset, numof_batches, phase):
        assert source_db_name not in self.source_db_name_to_source_db_inst
        assert source_db_name not in self.sources_dbs_name_to_offset_indexes_hash
        assert source_db_name not in self.sha_to_name_and_path

        self.sources_to_sources_db_dict[source_name] = DataSetSourceDB(source_db_name, self.batch_size, numof_batches, phase)
        self.source_db_name_to_source_db_inst[source_db_name] = self.sources_to_sources_db_dict[source_name]
        self.sources_dbs_name_to_offset_indexes_hash[source_db_name] = starting_offset
        # self.generate_hash_source_csv_db_sha(source_db_name)


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

        
    def generate_data_for_source(self, source_db_name, phase): # TODO change this code to be based on phase
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

        # assert source_db_name in self.sources_dict
        # source_db_inst = self.source_db_name[source_db_name]

        # starting_offset_index = self.sources_dbs_to_offset_indexes_train_hash[source_db_name]
        # starting_offset_index_predict = self.sources_dbs_to_offset_indexes_predict_hash[source_db_name]

        # number_of_samples_train = source_db_inst.get_batch_stop_idx(DataSetSourceDB.PHASE_TRAINING) * self.batch_size
        # number_of_samples_predict = source_db_inst.get_batch_stop_idx(DataSetSourceDB.PHASE_PREDICTION) * self.batch_size

        # df_train = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index_train, nrows=number_of_samples_train)
        # df_predict = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index_predict, nrows=number_of_samples_predict)

        # filename_train = f"{source_db_name}_{super().data_set_name}_training.csv"
        # filepath_train = f"{super().temp_path}/{filename_train}"
        # df_train.to_csv(filepath_train , index=False)
       
        # filename_predict = f"{source_db_name}_{super().data_set_name}_predict.csv"
        # filepath_predict = f"{super().temp_path}/{filename_predict}"
        # df_predict.to_csv(filepath_predict , index=False)

        # if phase == DataSetCsvDB.PHASE_PREDICTION:
        #     pass # generate data without labels


        # return filepath_train, filepath_predict,\
        #        filename_train, filename_predict,\
        #        number_of_samples_train, number_of_samples_predict
        pass