import requests
import globalVars as globe
import time
import sys
from experiment import *

class Transmitter:

    def __init__(self, mainServerAddress):
        # Addresses used throughout the module:
        self.mainServerAddress = mainServerAddress
        self.clientsTrainingAddress = self.mainServerAddress + '/clientsTraining'
        self.updateCSVAddress = self.mainServerAddress + '/updateCSV'
        self.startCastingAddress = self.mainServerAddress + '/startCasting'
        self.clientsPredictAddress = self.mainServerAddress + '/clientsPredict'
        self.statisticsAddress = self.mainServerAddress + '/statistics'

    def testPost(self, address, payloadNum):
        payload = {'test' : payloadNum}
        response = requests.post(address ,data = payload)
        print(response.ok, response.status_code, response.json())
        #Return true, if received: HTTP status code < 400
        #Return the HTTP status code for the response
        #Return the reponse in JSON format
        return(response.ok, response.status_code, response.json())

    def clientsTraining(self):
        print('Clients Training Phase')
        response = requests.post(self.clientsTrainingAddress, data='')
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)
        
    def updateCSV(self, currentPhase): # currentPhase is either "Training", "Prediction" or "Statistics". 
        print('Update CSV Phase')

        for source in globe.expFlow[currentPhase]: # Itterate over sources in accordance to current phase
            sourceName = source['source name']
            workersUnderSource = source['workers']
            csvPathForSource = source['CSV path']

            dataStr = f'{sourceName},{workersUnderSource},{csvPathForSource}'

            response = requests.post(self.updateCSVAddress, data=dataStr)
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def startCasting(self, phase): # numOfBatches, is no. of batches to request from the Main Server. On the other side, Batch size is found at the architecture JSOn, which is available at globe.components
        print('\nStart Casting Phase')

        if (phase=="Training"):
            batchesPerSource = globe.expFlow["Batches per source"]["Training"]
        elif (phase=="Prediction"):
            batchesPerSource = globe.expFlow["Batches per source"]["Prediction"]
        else:
            batchesPerSource = sys.maxsize

        dataStr = f"{globe.components.toString('s')},{batchesPerSource}" #Python's string format, {} are swapped by the variables in the brackets respectively.

        response = requests.post(self.startCastingAddress, data=dataStr)

        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def clientsPredict(self):
        print('Clients Predict Phase')
        response = requests.post(self.clientsPredictAddress, data='')
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def train(self):
        print('\nTraining - Starting...')

        globe.expResults.syncTrainingWithFlow()

        # 1 Ack for clientsTraining(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1 + len(globe.components.sources) 

        self.clientsTraining()

        self.updateCSV("Training")

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 
        
        # 1 Ack for startCasting():
        globe.pendingAcks += 1

        self.startCasting("Training") 

        while globe.pendingAcks > 0:
            time.sleep(0.05)
            pass 

        globe.expResults.remove0Tails()
        globe.multiProcQueue.put(globe.expResults)

    def predict(self):
        print('Prediction - Starting...')

        globe.expResults.syncPredicitionWithFlow()

        # 1 Ack for clientsPredict(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1 + len(globe.components.sources) 

        self.clientsPredict()

        self.updateCSV("Prediction")

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        # 1 Ack for startCasting():
        globe.pendingAcks += 1

        self.startCasting("Prediction")

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 
        
        globe.expResults.remove0Tails()
        globe.multiProcQueue.put(globe.expResults)

    def statistics(self):
        requests.post(self.statisticsAddress, data='')
    
if __name__ == "__main__":
    trans = Transmitter()
    trans.clientsTraining()

    '''
    def checkIfCsvInResults(self, resList, csv):
        for resDict in resList:
            if resDict['CSV path'] == csv:
                return True
        
        return False
    '''

    '''
    def ackTest(self):
        globe.pendingAcks += 1
        response = requests.get('http://127.0.0.1:8095' + '/testglobe')
        print(int(response.content))
    '''

    '''
    def testQueue(address):
        for i in range(0, 10):
            globe.q.put(testPost(address, i+1))

        empty = globe.q.empty()
    
        return empty
    
    def wait():
        while not globe.ackQueue.empty(): #While the queue is NOT empty
            pass
    '''
