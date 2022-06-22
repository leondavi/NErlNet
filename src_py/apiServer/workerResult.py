import numpy as np

class WorkerResult():

    def __init__(self, workerName, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked

        self.resList =  [None] * 100000 # Pre-allocate to improve efficiency]
        self.resCounter = 0

    def addResult(self, batch):
        self.resList[self.resCounter] = batch
        self.resCounter += 1

    def remove0Tail(self):
            self.resList = self.resList[:self.resCounter]
