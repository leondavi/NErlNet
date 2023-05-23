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
        IDX_WORKER = 0
        IDX_BATCH_SIZE = 1
        IDX_BATCHID = 2
        IDX_CSVNAME = 3
        IDX_PREDS = 4

        # Parsing the prediction received from Erlang, to initialize the class:
        self.worker = receivedPrediction[IDX_WORKER] 
        self.batchId = int(receivedPrediction[IDX_BATCHID])
        self.batchSize = int(receivedPrediction[IDX_BATCH_SIZE])
        self.csvName = receivedPrediction[IDX_CSVNAME]
        preds = receivedPrediction[IDX_PREDS].replace("[", "").replace("]", "").split(",")

        # Extract predictions from tensor:
        ## X = # of sample, Y = number of labels
        x,y,z = preds[:3]
        x, y, z = int(float(x)), int(float(y)), int(float(z))
        preds = preds[3:]
        self.predictions = [[] for i in range(x)]
        for i in range(x):
            for j in range(y):
                self.predictions[i].append(float(preds[i*y+j]))

        self.indexRange = ((self.batchSize*self.batchId), (self.batchSize*(self.batchId)+x)-1) 

    def fixOffset(self, offset):
        self.indexRange = (self.indexRange[0]+offset, self.indexRange[1]+offset)


## this keeps results per worker ordered
class WorkerResult():
    # csvWorked is name of csv, DEPRACATED.
    # 
    def __init__(self, workerName, phase, offset, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked
        self.resList = []

    def addResult(self, result):
        self.resList.append(result)

## Each source has its own CSV, this is what keeps results and workers ordered in data and experiment 
class CsvResult():

    def __init__(self, phase, csvName, workers, workedBySources = "Unknown", sourceNum = 0):
        self.name = csvName 
        self.workers = workers
        self.workedBySources = workedBySources
        self.workersResList = [] 
        self.phase = phase
        self.indexOffset = sourceNum * globe.sourceCSVIndex

    def addWorkers(self):
        for w in self.workers:
            newWorkerRes = WorkerResult(w, self.phase, self.indexOffset, self.workedBySources, self.name)
            self.workersResList.append(newWorkerRes)

    # TODO: define function to create worker confMatrix