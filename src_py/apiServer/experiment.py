################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import pandas as pd
import globalVars as globe
from definitions import *
from workerResult import *
from collections import OrderedDict
from NerlComDB import *
from NerlDB import *
from NerlDatasetDB import *

PARAM_NUMOF_FEATURES = "numof_features"
PARAM_NUMOF_LABELS = "numof_labels"
PARAM_FEATURES_HEADERS = "features_headers"
PARAM_LABELS_HEADERS = "labels_headers"
PARAM_DESCRIPTION = "description"
PARAM_LABELS = "labels"
PARAM_CSV_DB_PATH = "csv_db_path"
TRAINING_DBS = "training_dbs" # list of dicts describing source db params
PREDICTION_DBS = "prediction_dbs" # list of dicts describing source db params
PARAM_BATCH_SIZE = "batch_size"

# Source DB params:
PARAM_SOURCE_DB_NAME = "source_name"
PARAM_TARGET_WORKERS_NAMES_LIST = "target_workers_names_list"
PARAM_STARTING_OFFSET = "starting_offset"
PARAM_NUMOF_BATCHES = "numof_batches"


class Experiment():

    DATA_SOURCE_TYPE_CSV = 0
    DATA_SOURCE_TYPE_CAMERA = 1
    def __init__(self ,experiment_name = "Untitled", temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.name = experiment_name
        self.data_source_type = data_source_type
        self.trainingResList : list = [] # depracated
        self.predictionResList : list = [] # depracated
        self.exp_flow_json = None
        self.temp_data_path = temp_data_path
        
        self.nerl_db = NerlDB()
        self.dataset_db = None # will be initialized by parse_experiment_flow_json()
        self.nerl_comm_db = NerlComDB(globe.components)

    def parse_experiment_flow_json(self, json_path : str):
        # read json file
        with open(json_path) as json_file:
            self.exp_flow_json = json.load(json_file)
        # parse experiment flow json to dataset_db
        csv_dataset_path = self.exp_flow_json[PARAM_CSV_DB_PATH]
        csv_temp_path = self.temp_data_path
        batch_size = self.exp_flow_json[PARAM_BATCH_SIZE]
        self.dataset_db = DataSetCsvDB(csv_dataset_path, csv_temp_path, batch_size)
        # add training and prediction phases source_dbs to dataset_db
        phases_list = [TRAINING_DBS, PREDICTION_DBS]
        for phase in phases_list:
            for source_db_dict in self.exp_flow_json[phase]:
                source_db_name = source_db_dict[PARAM_SOURCE_DB_NAME]
                target_workers_names_list = source_db_dict[PARAM_TARGET_WORKERS_NAMES_LIST]
                starting_offset = source_db_dict[PARAM_STARTING_OFFSET]
                numof_batches = source_db_dict[PARAM_NUMOF_BATCHES]
                self.dataset_db.add_source_db(source_db_name, target_workers_names_list, starting_offset, starting_offset + numof_batches * batch_size, DataSetCsvDB.PHASE_TRAINING)
        

    def generate_skeleton_source_db_dict(self, source_db_name : str, workers_names_list : list, starting_offset : int, numof_batches : int):
        source_db_dict = OrderedDict([
            (PARAM_SOURCE_DB_NAME , source_db_name),
            (PARAM_TARGET_WORKERS_NAMES_LIST , workers_names_list),
            (PARAM_STARTING_OFFSET , starting_offset),
            (PARAM_NUMOF_BATCHES , numof_batches)
            ]
        )
        return source_db_dict

    def generate_json_skeleton(self, file_path : str):
        example_source_db_1 = self.generate_skeleton_source_db_dict("source_db_1", "w1,w2", 0, 10)
        example_source_db_2 = self.generate_skeleton_source_db_dict("source_db_2", "w3,w4", 0, 10)
        example_source_db_3 = self.generate_skeleton_source_db_dict("source_db_3", "w3,w4", 0, 10)
        example_source_db_4 = self.generate_skeleton_source_db_dict("source_db_4", "w5,w6", 0, 10)

        json_skeleton = OrderedDict([
            (PARAM_DESCRIPTION , "example csv data set for nerlnet"),
            (PARAM_NUMOF_FEATURES , 3),
            (PARAM_NUMOF_LABELS , 2),
            (PARAM_FEATURES_HEADERS , ['Feature1', 'Feature2', 'Feature3']),
            (PARAM_LABELS_HEADERS , ['Label1', 'Label2']),
            (PARAM_CSV_DB_PATH , "/tmp/nerlnet/data/example.csv"),
            (PARAM_BATCH_SIZE , 100),
            (TRAINING_DBS , [example_source_db_1, example_source_db_2]),
            (PREDICTION_DBS , [example_source_db_3, example_source_db_4])
        ])
        with open(file_path, 'w') as file:
            json.dump(json_skeleton, file, indent=4)
    
    def set_experiment_flow(self,expFlow):
        self.expFlow = expFlow
        self.labelsLen : int = len(self.expFlow["Labels"])
        self.labelNames = self.expFlow["Labels"]

    def syncTrainingWithFlow(self):
        for source in self.expFlow[globe.TRAINING_STR]:
            sourceWorkers = list(source["workers"].split(","))
            # TODO: simplify experiment json with only 1 CSV path and add to it globe.TRAINING_STR => csvName = self.expFlow [DATA_NAME+"_"+globe.TRAINING_STR] ("health_Training")
            newCsvRes = CsvResult(globe.TRAINING_STR, workedBySources = source, csvName = self.expFlow["CSV path"]+"_"+globe.TRAINING_STR.lower(), workers = sourceWorkers)
            newCsvRes.addWorkers()
            self.trainingResList.append(newCsvRes)

    def syncPredicitionWithFlow(self):
        for i, source in enumerate(self.expFlow[globe.PREDICTION_STR]):
            sourceWorkers = list(source["workers"].split(","))
            newCsvRes = CsvResult(globe.PREDICTION_STR, workedBySources = source, csvName = self.expFlow["CSV path"]+"_"+globe.PREDICTION_STR.lower(), workers = sourceWorkers, sourceNum=i)    #sourceNum used to devide test.csv to matching sample
            newCsvRes.addWorkers()
            self.predictionResList.append(newCsvRes)

    def emptyExp(self):
        self.trainingResList = []
        self.predictionResList = []

    def remove0Tails(self):
        for csv in self.trainingResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()

    def printExp(self):
        print(f"""Experiment Data:
        Data source:    {self.expFlow["CSV path"]}
        Batches to send per phase:
            Training:   {self.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.TRAINING_STR]}
            Prediction: {self.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.PREDICTION_STR]}
        """)

        # The prediction batches list is now NOT pre-allocated, cause batch size may differ.
        '''
        for csv in self.predictionResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()
        '''

    def get_labels_df(self) -> pd.DataFrame:
        filename = f"{self.predictionResList[0].name}_test.csv" 
        dirname = read_nerlconfig(NERLCONFIG_INPUT_DATA_DIR)
        labelsCsvPath = search_file(filename , dirname) # ? This is the best way to use search_file function?
        try:
            labelsCsvDf = pd.read_csv(labelsCsvPath, header=None)
        except OSError: 
            print("\nInvalid path\n")
            return None
        return labelsCsvDf
    
    def get_labels(self):
        labelsCsvDf = self.get_labels_df()
        if labelsCsvDf is None:
            return None
        labelsSeries = labelsCsvDf.iloc[:,-self.labelsLen:]
        return labelsSeries

    def get_results_labels(self) -> dict: # ! Needs better implementation and comments
        workers_results = {}
        labels = self.get_labels()
        for sourceCSV in self.predictionResList:
            for worker in sourceCSV.workersResList:
                predlabels = [[] for i in range(self.labelsLen)]
                trueLabels = [[] for i in range(self.labelsLen)] 
                for batchRes in worker.resList:
                    startSample , endSample = batchRes.indexRange
                    for index , sample in enumerate(range(startSample , endSample)):
                        for label in range(self.labelsLen):
                            truelabel = str(labels.iloc[sample,label])
                            if truelabel not in ('0' , '1'):
                                raise "invalid true label , must be 0 or 1"
                            predlabel = '1' if batchRes.predictions[index][label] > 0.5 else '0'
                            trueLabels[label].append(truelabel)
                            predlabels[label].append(predlabel)
                workers_results[worker.name] = [trueLabels , predlabels]
        return workers_results
    
    def get_workers_list(self) -> list:
        workersList = []
        for csvRes in self.predictionResList:
            for worker in csvRes.workers:
                workersList.append(worker)
        return workersList

