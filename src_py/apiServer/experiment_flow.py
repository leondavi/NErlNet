

from experiment_flow_defs import *
from logger import *
import pandas as pd
import globalVars as globe
from definitions import *
from workerResult import *
from collections import OrderedDict
from NerlComDB import *
from NerlDB import *
from NerlDatasetDB import *
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

    def get_name    ():
    pass
    def get_sources_str_list(self):
        return ",".join(self.source_pieces_dict.keys())

    def add_source_piece(self, source_piece : SourcePieceDS):
        if source_piece.source_name not in self.source_pieces_dict:
            self.source_pieces_dict[source_piece.source_name] = source_piece
        else: 
            LOG_ERROR(f"Source piece with name {source_piece.source_name} already exists in phase { self.phase}")
       
    def get_sources_pieces(self):
        return list(self.source_pieces_dict)

    def remove_source_piece(self, source_name: str): # Todo Ohad and Noa
        pass    


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

    def parse_experiment_flow_json(self, json_path : str):
        # read json file from nerlPlanner output
        with open(json_path) as json_file:
             self.exp_flow_json = json.load(json_file)
        # parse json and create experiment phases
        self.batch_size = self.exp_flow_json[EXPFLOW_BATCH_SIZE_FIELD]
        csv_file_path = self.exp_flow_json[EXPFLOW_CSV_FILE_PATH_FIELD]
        headers_row = self.exp_flow_json[EXPFLOW_HEADERS_NAMES_FIELD]
        num_of_features = self.exp_flow_json[EXPFLOW_NUM_OF_FEATURES_FIELD]
        num_of_labels = self.exp_flow_json[EXPFLOW_NUM_OF_LABELS_FIELD]
        self.set_csv_dataset(csv_file_path, num_of_features, num_of_labels, headers_row)
        phases_list = self.exp_flow_json[EXPFLOW_PHASES_FIELD]
        # Todo complete phases loop 


    def set_csv_dataset(self, csv_file_path : str,  num_of_features : int, num_of_labels : int, headers_row : bool):
        self.csv_dataset = CSVDataSet(csv_file_path, self.batch_size, num_of_features, num_of_labels, headers_row)  # Todo get num of features and labels from csv file

    def add_phase(self, phase_name : str, phase_type : str, sourcePieces : list):
        exp_phase = ExperimentPhase(phase_name)
        for source_piece in sourcePieces:
            exp_phase.add_source_piece(source_piece)
        self.exp_phase_list.append(exp_phase)
        
  


