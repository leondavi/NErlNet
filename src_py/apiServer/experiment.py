################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import globalVars as globe
from definitions import *
from networkComponents import NetworkComponents
from workerResult import *


class Experiment():

    def __init__(self ,experiment_name = "Untitled"):
        self.name = experiment_name
        self.trainingResList : list = []
        self.predictionResList : list = []
    
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

