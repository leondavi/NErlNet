################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
from workerResult import *
import numpy as np
import globalVars as globe
from networkComponents import NetworkComponents

class Experiment():

    def __init__(self ,experiment_name = "Untitled"):
        self.name = experiment_name
        self.trainingResList = []
        self.predictionResList = []
    
    def set_experiment_flow(self,expFlow):
        self.expFlow = expFlow

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