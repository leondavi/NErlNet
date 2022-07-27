###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
import numpy as np

class WorkerResult():

    def __init__(self, workerName, phase, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked

        if (phase == 'Training' or phase == 0):
            self.phase = 0
            self.resList =  [None] * 100000 # Pre-allocate to improve efficiency.

        elif (phase == 'Prediction' or phase == 1):
            self.phase = 1
            self.resList = [] #Not pre-allocated, cause batch size may differ.

        else:
            raise ValueError("Error in the WorkerResult class - <phase> should be either 'Training' 'Prediction'.")
        
        self.numOfResults = 0

    def addResult(self, result):
        if (self.phase == 0):
            self.resList[self.numOfResults] = result
        
        elif (self.phase == 1):
            self.resList.append(result)

        self.numOfResults += 1

    # Remove excess list cells in the pre-allocated list for the Training phase:
    def remove0Tail(self):
        if (self.phase == 0):
            self.resList = self.resList[:self.numOfResults]
