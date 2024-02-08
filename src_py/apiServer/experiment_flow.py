
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


class Experiment():

    DATA_SOURCE_TYPE_CSV = 0
    DATA_SOURCE_TYPE_CAMERA = 1
    def __init__(self ,experiment_name = "Untitled", temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.exp_name = experiment_name
        #self.gentrate_stats = 
        #self.csv_dataset = CsvDataset()
        self.exp_phase_list = []
        self.exp_flow_json = None

    def get_exp_name(self):
        return self.exp_name

    def get_exp_phase_list(self):
        return self.exp_phase_list

    def new_exp_phase(self, phase : str):
        exp_phase = ExperimentPhase(phase)
        self.exp_phase_list.append(exp_phase)

    #  def parse_experiment_flow_json(self, json_path : str):
    #     # read json file from nerlPlanner output
    #     with open(json_path) as json_file:
    #         self.exp_flow_json = json.load(json_file)
    #     # parse experiment flow json to dataset_db
    #     csv_dataset_path = self.exp_flow_json[PARAM_CSV_DB_PATH]
    #     csv_temp_path = self.temp_data_path
    #     batch_size = self.exp_flow_json[PARAM_BATCH_SIZE]
    #     self.dataset_db = DataSetCsvDB(csv_dataset_path, csv_temp_path, batch_size)
    #     # add training and prediction phases source_dbs to dataset_db
    #     phases_list = [TRAINING_DBS, PREDICTION_DBS]
    #     for phase in phases_list:
    #         for source_db_dict in self.exp_flow_json[phase]:
    #             source_db_name = source_db_dict[PARAM_SOURCE_DB_NAME]
    #             target_workers_names_list = source_db_dict[PARAM_TARGET_WORKERS_NAMES_LIST]
    #             starting_offset = source_db_dict[PARAM_STARTING_OFFSET]
    #             numof_batches = source_db_dict[PARAM_NUMOF_BATCHES]
    #             self.dataset_db.add_source_db(source_db_name, target_workers_names_list, starting_offset, starting_offset + numof_batches * batch_size, DataSetCsvDB.PHASE_TRAINING)

class ExperimentPhase():
    def __init__(self, phase : str):
        self.phase = phase # training/prediction
        self.nerl_comm_db = NerlComDB(globe.components)  # Todo check if this is the right place to put it
        self.nerl_model_db = NerlDB()  # Todo will change to NerlModelDB
        self.source_pieces_list = []  # list of SourcePieceDS
     
     def get_phase(self):
        return self.phase
