import numpy as np

class PredictBatch():

    def __init__(self, receivedPrediction):
        # Parsing the prediction received from Erlang, to initialize the class:
        self.worker = receivedPrediction[0]
        self.batchSize = receivedPrediction[-1]
        self.predictions = receivedPrediction[3:(3+self.batchSize)]
        self.batchId = receivedPrediction[self.batchSize]
        self.csvName = receivedPrediction[self.batchSize+1]

        self.indexRange = (self.batchSize * self.batchId, (self.batchSize * self.batchId) - 1)
