from csvResult import *
import numpy as np
import globalVars as globe

class Experiment():

    def __init__(self, expName = "Untitled"):
        self.name = expName
        self.trainingResList = []
        self.predictionResList = []

    def syncTrainingWithFlow(self):
       for source in globe.expFlow["Training"]:
           sourceWorkers = list(source["workers"].split(","))
           newCsvRes = CsvResult("Training", workedBySources = source, csvName = source["CSV path"], workers = sourceWorkers)
           newCsvRes.addWorkers()
           self.trainingResList.append(newCsvRes)

    def syncPredicitionWithFlow(self):
       for source in globe.expFlow["Prediction"]:
           sourceWorkers = list(source["workers"].split(","))
           newCsvRes = CsvResult("Prediction", workedBySources = source, csvName = source["CSV path"], workers = sourceWorkers)
           newCsvRes.addWorkers()
           self.predictionResList.append(newCsvRes)

    def emptyExp(self):
        self.trainingResList = []
        self.trainingResList = []

    def remove0Tails(self):
        for csv in self.trainingResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()

        for csv in self.predictionResList:
            for workerRes in csv.workersResList:
                workerRes.remove0Tail()