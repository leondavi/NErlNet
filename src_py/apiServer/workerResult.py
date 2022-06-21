import numpy as np

class WorkerResult():

    def __init__(self, workerName, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked

        self.resList =  np.zeros(100000) # Pre-allocate to improve efficiency
        self.resCounter = 0

    def addResult(self, res):
        self.resList[self.resCounter] = res
        self.resCounter += 1

    def remove0Tail(self):
        while self.resList[-1] == 0:
            self.resList = self.resList[:-1]
