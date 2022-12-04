###########################################################
##### Author: Dor Yarchi
# Nerlnet Copyright: © 2022
# Date: 27/07/2022
###########################################################
from csvResult import *
import numpy as np
import globalVars as globe

class Experiment():

    def __init__(self ,expName = "Untitled"):
        self.name = expName
        self.trainingResList = []
        self.predictionResList = []
    
    def set_experiment_flow(self,expFlow):
        self.expFlow = expFlow

    def syncTrainingWithFlow(self):
       for source in self.expFlow [globe.TRAINING_STR]:
           sourceWorkers = list(source["workers"].split(","))
           newCsvRes = CsvResult(globe.TRAINING_STR, workedBySources = source, csvName = source["CSV path"], workers = sourceWorkers)
           newCsvRes.addWorkers()
           self.trainingResList.append(newCsvRes)

    def syncPredicitionWithFlow(self):
       for source in self.expFlow[globe.PREDICTION_STR]:
           sourceWorkers = list(source["workers"].split(","))
           newCsvRes = CsvResult(globe.PREDICTION_STR, workedBySources = source, csvName = source["CSV path"], workers = sourceWorkers)
           newCsvRes.addWorkers()
           self.predictionResList.append(newCsvRes)

    def emptyExp(self):
        self.trainingResList = []
        self.trainingResList = []

    def remove0Tails(self):
        for csv in self.trainingResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()


        # The prediction batches list is now NOT pre-allocated, cause batch size may differ.
        '''
        for csv in self.predictionResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()
        '''