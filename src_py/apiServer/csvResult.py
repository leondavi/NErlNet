###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
from workerResult import *

class CsvResult():

    def __init__(self, phase, csvName, workers, workedBySources = "Unknown"):
        self.name = csvName 
        self.workers = workers
        self.workedBySources = workedBySources
        self.workersResList = [] 

        if (phase == 'Training'):
            self.phase = 0

        elif (phase == 'Prediction'):
            self.phase = 1

        else:
            raise ValueError("Error in the CsvResult class - <phase> should be either 'Training' 'Prediction'.")

    def addWorkers(self):
        for w in self.workers:
            newWorkerRes = WorkerResult(w, self.phase, self.workedBySources, self.name)
            self.workersResList.append(newWorkerRes)


    