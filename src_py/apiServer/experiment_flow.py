

from experiment_flow_defs import *
from logger import *
import pandas as pd
import globalVars as globe
from definitions import *
from workerResult import *
from collections import OrderedDict
from NerlComDB import *
from NerlModelDB import *
from nerl_csv_dataSet_db import *
# Todo check imports and remove unused ones

PARAM_CSV_DB_PATH = "csv_db_path"
PARAM_BATCH_SIZE = "batch_size"


class ExperimentPhase():
    def __init__(self, phase : str):
        self.phase = phase # training/prediction
        self.nerl_comm_db = NerlComDB(globe.components)  # Todo check if this is the right place to put it
        self.nerl_model_db = NerlModelDB()  # Todo will change to NerlModelDB
        self.source_pieces_dict = {}  # Dict of SourcePieceDS

    def get_phase(self):
        return self.phase

    def get_name(self):
    pass

    def get_sources_str_list(self):
        return ",".join(self.source_pieces_dict.keys())

    def add_source_piece(self, source_piece : SourcePieceDS):
        if source_piece.source_name not in self.source_pieces_dict:
            self.source_pieces_dict[source_piece.source_name] = source_piece
        else: 
            LOG_ERROR(f"Source piece with name {source_piece.source_name} already exists in phase { self.phase}")
       
    def get_sources_pieces(self):
        return list(self.source_pieces_dict.values())

    def remove_source_piece(self, source_name: str): 
        self.source_pieces_dict.pop(source_name)    


class ExperimentFlow():

    DATA_SOURCE_TYPE_CSV = 0
    DATA_SOURCE_TYPE_CAMERA = 1
    def __init__(self ,experiment_name = "Untitled", batch_size: int, temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.exp_name = experiment_name
        self.batch_size = 0
        self.csv_dataset = None
        self.exp_phase_list = []
        self.current_exp_phase_index = 0
        self.exp_flow_json = None

    def next_experiment_phase(self):
        self.current_exp_phase_index += 1

    def get_current_experiment_phase(self):
        return self.exp_phase_list[self.current_exp_phase_index]

    def get_exp_name(self):
        return self.exp_name

    def get_exp_phase_list(self):
        return self.exp_phase_list

    def generate_stats(self, experiment_phase: ExperimentPhase):
        pass
    # Todo implement this function : accuracy, confusion matrix, loss, etc.

    def parse_experiment_flow_json(self, json_path : str):
        # read json file from nerlPlanner output
        with open(json_path) as json_file:
             self.exp_flow_json = json.load(json_file)
        # parse json and create experiment phases
        self.exp_name = self.exp_flow_json[EXPFLOW_EXPERIMENT_NAME_FIELD]
        self.batch_size = self.exp_flow_json[EXPFLOW_BATCH_SIZE_FIELD]
        csv_file_path = self.exp_flow_json[EXPFLOW_CSV_FILE_PATH_FIELD]
        headers_row = self.exp_flow_json[EXPFLOW_HEADERS_NAMES_FIELD]
        num_of_features = self.exp_flow_json[EXPFLOW_NUM_OF_FEATURES_FIELD]
        num_of_labels = self.exp_flow_json[EXPFLOW_NUM_OF_LABELS_FIELD]
        self.set_csv_dataset(csv_file_path, num_of_features, num_of_labels, headers_row)
        phases_list = self.exp_flow_json[EXPFLOW_PHASES_FIELD]
        for phase in phases_list:
            phase_name = phase[EXPFLOW_PHASES_PHASE_NAME_FIELD]
            phase_type = phase[EXPFLOW_PHASES_PHASE_TYPE_FIELD]
            sourcePieces = phase[EXPFLOW_PHASES_PHASE_SOURCE_PIECES_FIELD]
            source_pieces_inst_list = []
            for source_piece in sourcePieces:
                # build source piece instant 
                source_name = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_SOURCE_NAME_FIELD]
                strating_sample = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_STRATING_SAMPLE_FIELD]
                num_of_batches = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD]
                workers = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_WORKERS_FIELD]
                source_piece_inst =  self.csv_dataset.generate_source_pieceDS(source_name, self.batch_size, phase_name, strating_sample, num_of_batches)
                source_piece_inst.build_workers_target(workers)
                source_piece_csv_file = self.csv_dataset.generate_source_pieceDs_csv_file(self.csv_dataset.get_csv_path(), source_piece_inst)
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet(source_piece_csv_file)
                source_pieces_inst_list.append(source_piece_inst)
                
            self.add_phase(phase_name, phase_type, source_pieces_inst_list)

    def set_csv_dataset(self, csv_file_path : str,  num_of_features : int, num_of_labels : int, headers_row : bool):
        self.csv_dataset = CSVDataSet(csv_file_path, self.batch_size, num_of_features, num_of_labels, headers_row)  # Todo get num of features and labels from csv file

    def add_phase(self, phase_name : str, phase_type : str, source_pieces_inst_list : list):
        exp_phase_inst = ExperimentPhase(phase_name)
        for source_piece_inst in source_pieces_inst_list:
            exp_phase_inst.add_source_piece(source_piece_inst)
        self.exp_phase_list.append(exp_phase_inst)
        
  


