from workerResult import *

class CsvResult():

    def __init__(self, phase, csvName, workers, workedBySources = "Unknown"):
        self.phase = phase
        self.name = csvName 
        self.workers = workers
        self.workedBySources = workedBySources
        self.workersResList = [] 

    def addWorkers(self):
        for w in self.workers:
            newWorkerRes = WorkerResult(w, self.workedBySources, self.name)
            self.workersResList.append(newWorkerRes)


    