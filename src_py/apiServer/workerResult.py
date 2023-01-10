###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
import numpy as np
import globalVars as globe

class WorkerResult():

    def __init__(self, workerName, phase, sourceWorked = "Unknown", csvWorked = "Unknown"):
        self.name  = workerName
        self.sourceWorked = sourceWorked
        self.csvWorked = csvWorked
        self.phase = phase

        if (phase == globe.TRAINING_STR):
            self.resList =  [None] * 100000 # Pre-allocate to improve efficiency.

        elif (phase == globe.PREDICTION_STR):
            self.resList = [] #Not pre-allocated, batch size may differ.

        self.numOfResults = 0

    def addResult(self, result):
        if (self.phase == globe.TRAINING_STR):
            self.resList[self.numOfResults] = result
        
        elif (self.phase == globe.PREDICTION_STR):
            self.resList.append(result)

        self.numOfResults += 1

    # Remove excess list cells in the pre-allocated list for the Training phase:
    def remove0Tail(self):
        if (self.phase == globe.TRAINING_STR):
            self.resList = self.resList[:self.numOfResults]
