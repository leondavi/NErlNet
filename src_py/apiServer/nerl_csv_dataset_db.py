
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
        self.target_workers = None # [worker_name1, worker_name2, ...]
        self.pointer_to_sourcePiece_CsvDataSet = None # pointer to the csv file that contains the source piece data
        self.filter_features_and_labels = [] 

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

    def get_target_workers(self):
        return self.target_workers
    
    def get_filter_features_and_labels(self):
        return self.filter_features_and_labels
    
    def update_target_workers(self, target_workers):
        self.target_workers = target_workers
    
    def set_filter_features_and_labels(self, filter_features_and_labels: list):
        self.filter_features_and_labels = filter_features_and_labels

    def set_pointer_to_sourcePiece_CsvDataSet(self, pointer_to_sourcePiece_CsvDataSet):
        self.pointer_to_sourcePiece_CsvDataSet = pointer_to_sourcePiece_CsvDataSet
    
class CsvDataSet():
    def __init__(self, csv_path, output_dir: str, batch_size, num_of_features, num_of_labels, headers_row: bool):
        self.csv_path = csv_path
        self.output_dir = output_dir
        #Todo throw exception if csv file does not exist and file path ends with .csv
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
        return floor(pd.read_csv(self.csv_path).shape[0] / self.batch_size)

    def generate_source_piece_ds(self, source_name : str, batch_size: int, phase : str, starting_offset: int, num_of_batches: int):
        assert batch_size > 0 
        assert num_of_batches >= 0 
        assert starting_offset >= 0
        assert phase == PHASE_TRAINING_STR or phase == PHASE_PREDICTION_STR
        assert (starting_offset + num_of_batches * batch_size) <= self.get_total_num_of_batches(), "starting_offset + num_of_batches * batch_size exceeds the total number of batches in the csv file"
        return SourcePieceDS(source_name, batch_size, phase, starting_offset, num_of_batches)
        
    def generate_source_piece_ds_csv_file(self, source_piece_ds_inst: SourcePieceDS, phase : str):
        skip_rows = source_piece_ds_inst.get_starting_offset()
        number_of_samples = source_piece_ds_inst.get_num_of_batches() * source_piece_ds_inst.get_batch_size()
        df = pd.read_csv(self.csv_path, skiprows = skip_rows, nrows = number_of_samples)
        df_features = df.iloc[:, :int(self.get_num_of_features())]  # from 0 column to num_of_features column (bun not including num_of_features column)  
        df_labels = df.iloc[:, int(self.get_num_of_features()):] # from num_of_features column to the end of the dataframe
        source_piece_file_path = f'{self.output_dir}/{source_piece_ds_inst.get_source_name()}_data.csv'
        if phase == PHASE_TRAINING_STR:  
            df.to_csv(source_piece_file_path, index = False)
        elif phase == PHASE_PREDICTION_STR:
            df_features.to_csv(source_piece_file_path, index = False)
        return source_piece_file_path
    
        
    

