###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
import numpy as np
import globalVars as globe

## this is what a worker returns / holds as data from network
class PredictBatch():

    def __init__(self, receivedPrediction):
        # Parsing the prediction received from Erlang, to initialize the class:
        self.worker = receivedPrediction[0] 
        self.batchSize = int(receivedPrediction[-1])
        self.csvName = receivedPrediction[-2]
        self.batchId = int(receivedPrediction[-3])

        # Extract predictions:
        preds = receivedPrediction[1].split(",")
        predsList = preds[3:]
        self.predictions = [float(pred) for pred in predsList]
        
        self.indexRange = ((self.batchSize*self.batchId) + 1, (self.batchSize*self.batchId))


## this keeps results per worker ordered
class WorkerResult():

    def __init__(self, workerName, phase, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked
        self.resList = []

    def addResult(self, result):
        self.resList.append(result)

## Each source has its own CSV, this is what keeps results and workers ordered in data and experiment 
class CsvResult():

    def __init__(self, phase, csvName, workers, workedBySources = "Unknown"):
        self.name = csvName 
        self.workers = workers
        self.workedBySources = workedBySources
        self.workersResList = [] 
        self.phase = phase

    def addWorkers(self):
        for w in self.workers:
            newWorkerRes = WorkerResult(w, self.phase, self.workedBySources, self.name)
            self.workersResList.append(newWorkerRes)
