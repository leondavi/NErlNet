
import os
from experiment_flow_defs import *
from logger import *
from definitions import *
from NerlComDB import *
from nerl_model_db import *
from nerl_csv_dataset_db import *
from events_sync import *
from networkComponents import *
from stats import * 
from statsTiles import *
from stats_aec import *
from experiment_phase import *

# Todo check imports and remove unused ones

PARAM_CSV_DB_PATH = "csv_db_path"
PARAM_BATCH_SIZE = "batch_size"

class ExperimentFlow():

    DATA_SOURCE_TYPE_CSV = 0
    DATA_SOURCE_TYPE_CAMERA = 1
    def __init__(self ,experiment_name, batch_size_dc: int, network_componenets: NetworkComponents, temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.exp_name = experiment_name
        self.exp_type = None
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

    # def next_experiment_phase(self):
    #     self.current_exp_phase_index += 1
    #     if self.current_exp_phase_index >= len(self.exp_phase_list) - 1:
    #         return False
    #     return True

    def get_current_experiment_phase(self):
        assert self.current_exp_phase_index < len(self.exp_phase_list) , "current experiment phase index is out of range"
        return self.exp_phase_list[self.current_exp_phase_index]

    def get_exp_name(self):
        return self.exp_name

    def get_exp_phase_list(self):
        return self.exp_phase_list
    
    def get_events_sync(self):
        return self.events_sync_inst
    
    def get_csv_dataset(self):
        return self.csv_dataset

    def generate_stats(self, experiment_phase = None) -> Stats:
        if experiment_phase is None:
            experiment_phase = self.get_current_experiment_phase() 
        return Stats(experiment_phase)
    
    def generate_stats_tiles(self, experiment_phase = None) -> StatsTiles:
        if experiment_phase is None:
            experiment_phase = self.get_current_experiment_phase() 
        return StatsTiles(experiment_phase)
    
    def generate_stats_aec(self, stats: Stats) -> Stats:
        assert stats is not None , "stats is None"
        # TODO add assertion of AEC experiment type
        return StatsAEC(stats)

    def merge_stats(self, stats_list: list) -> Stats:
        pass

    def parse_experiment_flow_json(self, json_path : str, override_csv_path = ""):
        '''
        json path is the path to the json file that was created by the nerlPlanner
        override_csv_path is the path to the csv file that will be used instead of the one in the json file
        if it is empty the csv file path from the json file will be used
        '''
        # read experimentFlow json file
        with open(json_path) as json_file:
            self.exp_flow_json = json.load(json_file)
        # parse json and create experiment phases
        self.exp_name = self.exp_flow_json[EXPFLOW_EXPERIMENT_NAME_FIELD]
        assert self.exp_flow_json[EXPFLOW_EXPERIMENT_TYPE_FIELD] , "experiment type is missing"
        self.exp_type = self.exp_flow_json[EXPFLOW_EXPERIMENT_TYPE_FIELD]
        self.batch_size = self.exp_flow_json[EXPFLOW_BATCH_SIZE_FIELD]
        assert self.batch_size == self.batch_size_dc, "Make sure the batch size field in the DC json and the Exp_flow json are the same"
        csv_file_path = self.exp_flow_json[EXPFLOW_CSV_FILE_PATH_FIELD] if override_csv_path == "" else override_csv_path
        headers_row = self.exp_flow_json[EXPFLOW_HEADERS_NAMES_FIELD].split(",")
        num_of_features = self.exp_flow_json[EXPFLOW_NUM_OF_FEATURES_FIELD]
        num_of_labels = self.exp_flow_json[EXPFLOW_NUM_OF_LABELS_FIELD]
        self.set_csv_dataset(csv_file_path, num_of_features, num_of_labels, headers_row)
        phases_list = self.exp_flow_json[EXPFLOW_PHASES_FIELD]
        phases_names_dict= {}
        phase_index = 1
        for phase in phases_list:
            phase_name = phase[EXPFLOW_PHASES_PHASE_NAME_FIELD]
            assert phase_name not in phases_names_dict , "check for duplicate phase names" 
            phases_names_dict.update({phase_name: phase_index})
            phase_index += 1
            phase_type = phase[EXPFLOW_PHASES_PHASE_TYPE_FIELD]
            sourcePieces = phase[EXPFLOW_PHASES_PHASE_SOURCE_PIECES_FIELD]
            source_pieces_inst_list = []
            for source_piece in sourcePieces:
                # build source piece instant 
                source_name = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_SOURCE_NAME_FIELD]
                starting_sample = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_STARTING_SAMPLE_FIELD])
                num_of_batches = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD])
                workers = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_WORKERS_FIELD]
                nerltensor_type = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NERLTENSOR_TYPE_FIELD]
                source_piece_inst = self.csv_dataset.generate_source_piece_ds(source_name, self.batch_size, phase_type, starting_sample, num_of_batches, nerltensor_type)
                source_piece_inst.update_target_workers(workers)
                source_piece_csv_file = self.csv_dataset.generate_source_piece_ds_csv_file(source_piece_inst, phase_type, phase_name)
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet(source_piece_csv_file)
                source_pieces_inst_list.append(source_piece_inst)
            LOG_INFO(f"phase {phase_name} source pieces parsed and generated.")
                
            self.add_phase(phase_name, phase_type, source_pieces_inst_list, num_of_features)


    def generate_experiment_flow_skeleton(self): 
        # Todo check with david if we need this function
        # for user to fill in the details
        experimentName = ""
        batch_size = 0
        csv_file_path = ""
        num_of_features = 0
        num_of_labels = 0

    def set_csv_dataset(self, csv_file_path : str,  num_of_features : int, num_of_labels : int, headers_row : list):
        self.csv_dataset = CsvDataSet(csv_file_path, self.temp_data_path ,self.batch_size, num_of_features, num_of_labels, headers_row)  # Todo get num of features and labels from csv file

    def add_phase(self, name : str, phase_type : str, source_pieces_inst_list : list, num_of_features : str):
        exp_phase_inst = ExperimentPhase(self.exp_name, self.exp_type, name, phase_type, self.network_componenets, num_of_features)
        for source_piece_inst in source_pieces_inst_list:
            exp_phase_inst.add_source_piece(source_piece_inst)
        self.exp_phase_list.append(exp_phase_inst)

        
    def print(self):
        
        LOG_INFO(f"Experiment name: {self.exp_name}")
        LOG_INFO(f"Batch size: {self.batch_size}")
        #LOG_INFO(f"Temp data path: {self.temp_data_path}")
        #LOG_INFO(f"CSV dataset: {self.csv_dataset.get_csv_file_path()}")
        LOG_INFO(f"Number of features: {self.csv_dataset.get_num_of_features()}")
        LOG_INFO(f"Number of labels: {self.csv_dataset.get_num_of_labels()}")
        LOG_INFO("")
        LOG_INFO("Phases:")
        for phase in self.exp_phase_list:
            LOG_INFO(f"   Phase name: {phase.get_name()}")
            LOG_INFO(f"   Phase type: {phase.get_phase_type()}")
            LOG_INFO(f"   Sources: {phase.get_sources_str_list()}")
            LOG_INFO("")
            LOG_INFO("    Source pieces:")
            for source_piece in phase.get_sources_pieces():
                LOG_INFO(f"         Source name: {source_piece.get_source_name()}")
                LOG_INFO(f"         Batch size: {source_piece.get_batch_size()}")
                LOG_INFO(f"         Phase: {source_piece.get_phase()}")
                LOG_INFO(f"         Starting offset: {source_piece.get_starting_offset()}")
                LOG_INFO(f"         Number of batches: {source_piece.get_num_of_batches()}")
                LOG_INFO(f"         Workers target: {source_piece.get_target_workers()}")
                LOG_INFO("      ----------------------")
                LOG_INFO("")