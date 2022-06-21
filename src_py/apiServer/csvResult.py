from workerResult import *

class CsvResult():

    def __init__(self, phase, csvName, workers, workedBySources = "Unknown"):
        self.phase = phase
        self.name = csvName 
        self.workers = workers
        self.workedBySources = workedBySources
        
        self.workersResList = [] * len(workers)
        CsvResult.addWorkers()

    def addWorkers(self, workerRes):
        for idx, w in enumerate(self.workers):
            newWorkerRes = WorkerResult(w, self.workedBySource, self.csvName)
            self.workersResList[idx] = newWorkerRes



    