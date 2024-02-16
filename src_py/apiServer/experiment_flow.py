

from experiment_flow_defs import *
from logger import *
import pandas as pd
import globalVars as globe
from definitions import *
from workerResult import *
from collections import OrderedDict
from NerlComDB import *
from nerl_model_db import *
from nerl_csv_dataset_db import *
from events_sync import *
from networkComponents import *
import os

# Todo check imports and remove unused ones

PARAM_CSV_DB_PATH = "csv_db_path"
PARAM_BATCH_SIZE = "batch_size"


class ExperimentPhase():
    def __init__(self, name : str, phase_type: str, network_componenets: NetworkComponents):
        self.name = name 
        self.phase_type = phase_type # training/prediction
        assert self.phase_type in [PHASE_TRAINING_STR, PHASE_PREDICTION_STR]
        self.nerl_comm_db = NerlComDB(network_componenets)  
        self.nerl_model_db = NerlModelDB(self.phase_type)
        self.source_pieces_dict = {}  # Dict of SourcePieceDS

    def get_phase_type(self):
        return self.phase_type

    def get_name(self):
        return self.name

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
    def __init__(self ,experiment_name, batch_size_dc: int, network_componenets: NetworkComponents, temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.exp_name = experiment_name
        self.batch_size_dc = batch_size_dc
        self.batch_size = None  # batch size from parsed exp_flow_json
        self.network_componenets = network_componenets
        self.temp_data_path = f"{temp_data_path}/{self.exp_name}"
        if not os.path.exists(self.temp_data_path):
            os.makedirs(self.temp_data_path)
        self.csv_dataset = None
        self.exp_phase_list = []
        self.current_exp_phase_index = 0
        self.exp_flow_json = None
        self.events_sync_inst = EventSync()

    def next_experiment_phase(self):
        self.current_exp_phase_index += 1
        if self.current_exp_phase_index >= len(self.exp_phase_list) - 1:
            return False
        return True

    def get_current_experiment_phase(self):
        return self.exp_phase_list[self.current_exp_phase_index]

    def get_exp_name(self):
        return self.exp_name

    def get_exp_phase_list(self):
        return self.exp_phase_list
    
    def get_events_sync(self):
        return self.events_sync_inst
    
    def get_csv_dataset(self):
        return self.csv_dataset

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
        assert self.batch_size == self.batch_size_dc
        csv_file_path = self.exp_flow_json[EXPFLOW_CSV_FILE_PATH_FIELD]
        headers_row = self.exp_flow_json[EXPFLOW_HEADERS_NAMES_FIELD].split(",")
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
                starting_sample = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_STARTING_SAMPLE_FIELD])
                num_of_batches = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD])
                workers = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_WORKERS_FIELD]   # Todo fix that
                source_piece_inst =  self.csv_dataset.generate_source_piece_ds(source_name, self.batch_size, phase_type, starting_sample, num_of_batches)
                source_piece_inst.build_workers_target(workers)
                source_piece_csv_file = self.csv_dataset.generate_source_piece_ds_csv_file(self.csv_dataset.get_csv_path(), source_piece_inst)
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet(source_piece_csv_file)
                source_pieces_inst_list.append(source_piece_inst)
                
            self.add_phase(phase_name, phase_type, source_pieces_inst_list)


    def generate_experiment_flow_skeleton(self):
        # Todo implement this function 
        # for user to fill in the details
        experimentName = ""
        batch_size = 0
        csv_file_path = ""

        pass
    def set_csv_dataset(self, csv_file_path : str,  num_of_features : int, num_of_labels : int, headers_row : bool):
        self.csv_dataset = CsvDataSet(csv_file_path, self.temp_data_path ,self.batch_size, num_of_features, num_of_labels, headers_row)  # Todo get num of features and labels from csv file

    def add_phase(self, name : str, phase_type : str, source_pieces_inst_list : list):
        exp_phase_inst = ExperimentPhase(name, phase_type, self.network_componenets)
        for source_piece_inst in source_pieces_inst_list:
            exp_phase_inst.add_source_piece(source_piece_inst)
        self.exp_phase_list.append(exp_phase_inst)
        
    def print(self):
        #Todo change print to LOG_INFO
        print(f"Experiment name: {self.exp_name}")
        print(f"Batch size: {self.batch_size}")
        # for phase in self.exp_phase_list:
        #     print(f"Phase name: {phase.get_name()}")
        #     print(f"Phase type: {phase.get_phase_type()}")
        #     print(f"Sources: {phase.get_sources_str_list()}")
        #     print("Source pieces:")
        #     for source_piece in phase.get_sources_pieces():
        #         print(f"Source name: {source_piece.get_source_name()}")
        #         print(f"Batch size: {source_piece.get_batch_size()}")
        #         print(f"Phase: {source_piece.get_phase()}")
        #         print(f"Starting offset: {source_piece.get_starting_offset()}")
        #         print(f"Number of batches: {source_piece.get_num_of_batches()}")
        #         print(f"Workers target: {source_piece.get_workers_target()}")
        #         print("")


