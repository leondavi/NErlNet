import numpy as np

class WorkerResult():

    def __init__(self, workerName, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked

        self.resList =  [None] * 100000 # Pre-allocate to improve efficiency
        self.numOfResults = 0

    def addResult(self, result):
        self.resList[self.numOfResults] = result
        self.numOfResults += 1

    def remove0Tail(self):
            self.resList = self.resList[:self.numOfResults]
