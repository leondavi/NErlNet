
from definitions import PHASE_TRAINING_STR, PHASE_PREDICTION_STR
import pandas as pd
from math import floor

class SourcePieceDS():
    def __init__(self, source_name : str, batch_size, phase : str, starting_offset = 0, num_of_batches = 0):
        self.source_name = source_name
        self.batch_size = batch_size
        self.phase = phase
        self.starting_offset = starting_offset  # given as index of csv rows
        self.num_of_batches = num_of_batches
        self.workers_target = [] # [worker_name1, worker_name2, ...]
        self.pointer_to_sourcePiece_CsvDataSet= None # pointer to the csv file that contains the source piece data

    def get_source_name(self):
        return self.source_name
    
    def get_batch_size(self):
        return self.batch_size

    def get_phase(self):
        return self.phase

    def get_starting_offset(self):
        return self.starting_offset 

    def get_num_of_batches(self):
        return self.num_of_batches

    def get_workers_target(self):
        return self.workers_target
    
    def build_workers_target(self, workers_target: list):
        self.workers_target = workers_target

    def set_pointer_to_sourcePiece_CsvDataSet(self, pointer_to_sourcePiece_CsvDataSet):
        self.pointer_to_sourcePiece_CsvDataSet = pointer_to_sourcePiece_CsvDataSet
    
class CsvDataSet():
    def __init__(self, csv_path, batch_size, num_of_features, num_of_labels, headers_row: bool):
        self.csv_path = csv_path
        self.batch_size = batch_size
        self.num_of_features = num_of_features
        self.num_of_labels = num_of_labels
        self.headers_row = headers_row

    def get_csv_path(self):
        return self.csv_path

    def get_batch_size(self):
        return self.batch_size
    
    def get_num_of_features(self):
        return self.num_of_features

    def get_num_of_labels(self):
        return self.num_of_labels

    def get_total_num_of_batches(self):
        return floor(pd.read_csv(self.csv_path, header = self.headers_row).shape[0] / self.batch_size)

    def generate_source_piece_ds(self, source_name : str, batch_size: int, phase : str, starting_offset: int, num_of_batches: int):
        assert batch_size > 0 
        assert num_of_batches >= 0 
        assert starting_offset >= 0
        assert phase == PHASE_TRAINING_STR or phase == PHASE_PREDICTION_STR
        assert (starting_offset + num_of_batches * batch_size) <= self.get_total_num_of_batches()
        return SourcePieceDS(source_name, batch_size, phase, starting_offset, num_of_batches)
        
    def generate_source_piece_ds_csv_file(self, csv_file_path : str, source_pieceDS_inst: SourcePieceDS):
        # Todo Ohad&Noa
        # df_train = pd.df.read_csv(self.csv_dataset_path, skiprows=starting_offset_index_train, nrows=number_of_samples_train)
        #return path for source piece csv file
        pass 
    

