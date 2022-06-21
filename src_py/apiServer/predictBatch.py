import numpy as np

class PredictBatch():

    def __init__(self, worker, predictions, batchId, csvName, batchSize):
        self.worker = worker
        self.predictions = predictions
        self.batchId = batchId
        self.csvName = csvName
        self.batchSize = batchSize