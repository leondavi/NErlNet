###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
import requests
import globalVars as globe
import time
import sys
import os
from experiment import *

class Transmitter:

    def __init__(self, mainServerAddress):
        # Addresses used throughout the module:
        self.mainServerAddress = mainServerAddress
        self.sourceInitAddr = self.mainServerAddress + '/sourceInit'
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

        #split data by sources and send to mainServer:
        for root, dirnames, filenames in os.walk(globe.INPUT_DATA_PATH):
            for filename in filenames:
                if filename == globe.experiment_flow_global.expFlow['CSV path']+"_"+currentPhase.lower()+".csv":
                    csvfile = open(os.path.join(root, filename), 'r').readlines()
                    break

        linesPerSource = int(len(csvfile)/len(globe.components.sources))

        SourceData = []
        for row in range(0,len(csvfile),linesPerSource):
            SourceData.append(csvfile[row:row+linesPerSource])

        i=0
        for source in globe.experiment_flow_global.expFlow[currentPhase]: # Itterate over sources in accordance to current phase
            sourceName = source['source name']
            workersUnderSource = source['workers']
            #csvPathForSource = source['CSV path']
            SourceStr = ""
            for Line in SourceData[i]:
                SourceStr += Line
            #dataStr = f'{sourceName},{workersUnderSource},{csvPathForSource}'
            dataStr = f'{sourceName}#{workersUnderSource}#{SourceStr}'
            response = requests.post(self.updateCSVAddress, data=dataStr)
            i+=1

        print("Data sent to sources")
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

        return linesPerSource

    def startCasting(self, phase):
        print('\nStart Casting Phase')

        # numOfBatches, is no. of batches to request from the Main Server. On the other side, Batch size is found at the architecture JSOn, which is available at globe.components
        if (phase==globe.TRAINING_STR):
            batchesPerSource = globe.experiment_flow_global.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.TRAINING_STR]
        elif (phase==globe.PREDICTION_STR):
            batchesPerSource = globe.experiment_flow_global.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.PREDICTION_STR]
        else:
            batchesPerSource = sys.maxsize

        dataStr = f"{globe.components.toString('s')},{batchesPerSource}" #sources, batches

        response = requests.post(self.startCastingAddress, data=dataStr) #startCasting to sources

        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def clientsPredict(self):
        print('Clients Predict Phase')
        response = requests.post(self.clientsPredictAddress, data='')
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def train(self):
        print('\nTraining - Starting...')

        globe.experiment_flow_global.syncTrainingWithFlow()

        # 1 Ack for clientsTraining(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1

        self.clientsTraining()
        
        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 
        
        # 1 Ack for startCasting():
        globe.pendingAcks += 1

        self.startCasting(globe.TRAINING_STR) 

        while globe.pendingAcks > 0:
            time.sleep(0.05)
            pass 

        #globe.experiment_flow_global.remove0Tails()
        globe.multiProcQueue.put(globe.experiment_flow_global)


    def contPhase(self, phase):     # phase can be train/training no matter capitals, otherwise predict
        print("starting additional training")

        globe.pendingAcks += 1
        if(phase.lower().startswith("train")):      # accepts train / training / etc...
            self.clientsTraining()
            phase = globe.TRAINING_STR
        else:
            self.clientsPredict()
            phase = globe.PREDICTION_STR

        while globe.pendingAcks > 0:
            time.sleep(0.05)
            pass 

        globe.pendingAcks += 1

        self.startCasting(phase) 

        while globe.pendingAcks > 0:
            time.sleep(0.05)
            pass 

        globe.multiProcQueue.put(globe.experiment_flow_global)

    def predict(self):
        print('Prediction - Starting...')

        globe.experiment_flow_global.syncPredicitionWithFlow()

        # 1 Ack for clientsPredict(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1

        self.clientsPredict()

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        # 1 Ack for startCasting():
        globe.pendingAcks += 1

        self.startCasting(globe.PREDICTION_STR)

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 
        
        # globe.experiment_flow_global.remove0Tails()
        globe.multiProcQueue.put(globe.experiment_flow_global)

    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics')
    
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
