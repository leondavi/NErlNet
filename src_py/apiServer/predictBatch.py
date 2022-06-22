import numpy as np

class PredictBatch():

    def __init__(self, receivedPrediction):
        # Parsing the prediction received from Erlang, to initialize the class:
        self.worker = receivedPrediction[0] 
        self.batchSize = int(receivedPrediction[-1])
        self.csvName = receivedPrediction[-2]
        self.batchId = int(receivedPrediction[-3])

        # Extract predictions:
        preds = receivedPrediction[1].split(",")
        predsList = preds[3:]
        self.predictions = [float(pred) for pred in predsList]
        
        self.indexRange = (self.batchSize * self.batchId, (self.batchSize * self.batchId) - 1)

