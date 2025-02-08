from definitions import PHASE_TRAINING_STR, PHASE_PREDICTION_STR, NERLTENSOR_TYPE_LIST
import pandas as pd
from math import ceil
import os

class SourcePieceDS():
    def __init__(self, csv_dataset_parent, source_name : str, batch_size, phase : str, starting_offset = 0, num_of_batches = 0, nerltensor_type: str = 'float', num_of_features = 0, num_of_labels = 0):
        self.source_name = source_name
        self.batch_size = batch_size
        self.num_of_features = num_of_features
        self.num_of_labels = num_of_labels
        self.phase = phase
        self.starting_offset = starting_offset  # given as index of csv rows
        self.num_of_batches = num_of_batches
        self.target_workers = None # [worker_name1, worker_name2, ...]
        self.pointer_to_sourcePiece_CsvDataSet = None # pointer to the csv file that contains the source piece data
        self.pointer_to_sourcePiece_CsvDataSet_labels = None # pointer to the csv file that contains the source piece labels
        self.filter_features_and_labels = [] 
        self.csv_dataset_parent = csv_dataset_parent
        self.nerltensor_type = nerltensor_type

    def get_source_name(self):
        return self.source_name
    
    def get_batch_size(self):
        return self.batch_size
    
    def get_num_of_features(self):
        return self.num_of_features
    
    def get_num_of_labels(self):
        return self.num_of_labels

    def get_phase(self):
        return self.phase

    def get_starting_offset(self):
        return self.starting_offset 

    def get_num_of_batches(self):
        return self.num_of_batches

    def get_target_workers(self):
        return self.target_workers
    
    def get_nerltensor_type(self):
        return self.nerltensor_type
    
    def get_filter_features_and_labels(self):
        return self.filter_features_and_labels
    
    def get_pointer_to_sourcePiece_CsvDataSet(self):
        assert self.pointer_to_sourcePiece_CsvDataSet is not None, "pointer_to_sourcePiece_CsvDataSet is None"
        return self.pointer_to_sourcePiece_CsvDataSet
    
    def get_pointer_to_sourcePiece_CsvDataSet_labels(self):
        assert self.pointer_to_sourcePiece_CsvDataSet_labels is not None, "pointer_to_sourcePiece_CsvDataSet_labels is None"
        return self.pointer_to_sourcePiece_CsvDataSet_labels

    def get_csv_dataset_parent(self):
        return self.csv_dataset_parent

    def update_target_workers(self, target_workers):
        self.target_workers = target_workers
    
    def set_filter_features_and_labels(self, filter_features_and_labels: list):
        self.filter_features_and_labels = filter_features_and_labels

    def set_pointer_to_sourcePiece_CsvDataSet(self, pointer_to_sourcePiece_CsvDataSet):
        self.pointer_to_sourcePiece_CsvDataSet = pointer_to_sourcePiece_CsvDataSet
    
    def set_pointer_to_sourcePiece_CsvDataSet_labels(self, pointer_to_sourcePiece_CsvDataSet_labels):
        self.pointer_to_sourcePiece_CsvDataSet_labels = pointer_to_sourcePiece_CsvDataSet_labels


class CsvDataSet():
    def __init__(self, csv_path, output_dir: str, batch_size, num_of_features, num_of_labels, headers_row: list):
        self.csv_path = csv_path
        assert self.csv_path.endswith('.csv'), "csv_path should end with '.csv'"
        assert os.path.exists(self.csv_path), "csv_path does not exist"
        self.output_dir = output_dir
        self.batch_size = batch_size
        self.num_of_features = int(num_of_features)
        self.num_of_labels = int(num_of_labels)
        self.headers_row = headers_row
        self.df = pd.read_csv(self.csv_path, header = None) 
        assert self.df.shape[1] == (self.num_of_features + self.num_of_labels), "Experiment Flow JSON #Features + #Labels mismatched with CSV #Cols"

    def get_csv_path(self):
        return self.csv_path

    def get_batch_size(self):
        return self.batch_size
    
    def get_num_of_features(self):
        return self.num_of_features

    def get_num_of_labels(self):
        return self.num_of_labels
    
    def set_num_of_labels(self, num_of_labels):
        self.num_of_labels = num_of_labels

    def get_total_num_of_batches(self): 
        # ! Not always using the whole csv! (should be calculated by the source pieces offsets)
        return ceil(self.df.shape[0] / self.batch_size)

    def get_total_num_of_samples(self):
        # ! Not always using the whole csv! (should be calculated by the source pieces offsets)
        return self.df.shape[0] + 1 # +1 for sample 0 which is the header row
    
    def get_headers_row(self):
        return self.headers_row
    
    def generate_source_piece_ds(self, source_name : str, batch_size: int, phase : str, starting_offset: int, num_of_batches: int, nerltensor_type: str):
        assert num_of_batches >= 0 
        assert starting_offset >= 0
        assert phase == PHASE_TRAINING_STR or phase == PHASE_PREDICTION_STR , "phase should be either 'training' or 'prediction'"
        assert (starting_offset + num_of_batches * batch_size) <= self.get_total_num_of_samples(), "starting_offset + num_of_batches * batch_size exceeds the total number of samples in the csv file"
        assert nerltensor_type in NERLTENSOR_TYPE_LIST, "nerltensor_type is not in NERLTENSOR_TYPE_LIST"
        return SourcePieceDS(self, source_name, batch_size, phase, starting_offset, num_of_batches, nerltensor_type, self.num_of_features, self.num_of_labels)
        
    def generate_source_piece_ds_csv_file(self, source_piece_ds_inst: SourcePieceDS, phase_type : str, phase_name : str):
        skip_rows = source_piece_ds_inst.get_starting_offset()
        number_of_samples = source_piece_ds_inst.get_num_of_batches() * source_piece_ds_inst.get_batch_size()
        df = self.df[skip_rows:(skip_rows + number_of_samples)] # slicing creates a copy of the data
        df_features = df.iloc[:, :int(self.get_num_of_features())]  # from 0 column to num_of_features column (bun not including num_of_features column)  
        source_piece_file_path = f'{self.output_dir}/{source_piece_ds_inst.get_source_name()}_data_{phase_name}.csv'
        if phase_type == PHASE_TRAINING_STR:  
            df_train = df.iloc[:, :int(self.get_num_of_features()) + int(self.get_num_of_labels())]  # from 0 column to num_of_features + num_of_labels column (bun not including num_of_features + num_of_labels column)
            df_train.to_csv(source_piece_file_path, index = False, header = False)
        elif phase_type == PHASE_PREDICTION_STR:
            df_features.to_csv(source_piece_file_path, index = False, header = False)
        return source_piece_file_path
    
    def genrate_source_piece_ds_csv_file_labels(self, source_piece_ds_inst: SourcePieceDS, phase: str):
        if phase == PHASE_PREDICTION_STR:  # only for prediction phase
            skip_rows = source_piece_ds_inst.get_starting_offset()
            number_of_samples = source_piece_ds_inst.get_num_of_batches() * source_piece_ds_inst.get_batch_size()
            df = self.df[skip_rows:(skip_rows + number_of_samples)]
            df_labels = df.iloc[:, int(self.get_num_of_features()):int(self.get_num_of_features()) + int(self.get_num_of_labels())] # from num_of_features column to the end of the dataframe
            df_labels.columns = self.headers_row
            source_piece_file_path_labels = f'{self.output_dir}/{source_piece_ds_inst.get_source_name()}_data_labels.csv'
            df_labels.to_csv(source_piece_file_path_labels, index = False)
            return source_piece_file_path_labels
        return None

