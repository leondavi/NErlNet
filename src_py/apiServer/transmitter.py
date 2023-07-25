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

def waitForAck():
    while globe.pendingAcks > 0:
        time.sleep(0.005)

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
        # 1 Ack for clientsTraining()
        globe.pendingAcks = 1
        print('Clients Training Phase')
        response = requests.post(self.clientsTrainingAddress, data='')
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def clientsPredict(self):
        globe.pendingAcks = 1
        print('Clients Predict Phase')
        response = requests.post(self.clientsPredictAddress, data='')
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)
        
    def updateCSV(self, currentPhase): # currentPhase is either "Training", "Prediction" or "Statistics". 
        print('Update CSV Phase')

        #split data by sources and send to mainServer:
        for root, dirnames, filenames in os.walk(globe.INPUT_DATA_PATH):
            for filename in filenames:
                if filename == f"{globe.experiment_flow_global.expFlow['CSV path']}_{currentPhase.lower()}.csv":
                    with open(os.path.join(root, filename), 'r') as f:
                        csvfile = f.read()
                    break

        SourceData = []
        if globe.CSVsplit == 2:      ## send entire file to sources
            linesPerSource = 0
            
            for source in globe.experiment_flow_global.expFlow[currentPhase]: # Itterate over sources in accordance to current phase
                sourceName = source['source name']
                workersUnderSource = source['workers']

                response = requests.post(self.updateCSVAddress, data=f'{sourceName}#{workersUnderSource}#{csvfile}')

        else:                   ## split file and send to sources
            linesPerSource = int(len(csvfile)/len(globe.components.sources))
            for row in range(0,len(csvfile),linesPerSource):
                SourceData.append(csvfile[row:row+linesPerSource])

            for i,source in enumerate(globe.experiment_flow_global.expFlow[currentPhase]): # Itterate over sources in accordance to current phase
                sourceName = source['source name']
                workersUnderSource = source['workers']
                SourceStr = ""
                for Line in SourceData[i]:
                    SourceStr += Line
                dataStr = f'{sourceName}#{workersUnderSource}#{SourceStr}'

                response = requests.post(self.updateCSVAddress, data=dataStr)

        print("Data sent to sources")
        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

        globe.sourceCSVIndex=linesPerSource

    def startCasting(self, phase):
        print('\nStart Casting Phase')

        # 1 Ack for startCasting():
        globe.pendingAcks = 1 

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

    def train(self):
        print('\nTraining - Starting...')

        globe.experiment_flow_global.syncTrainingWithFlow()

        self.clientsTraining()
        
        waitForAck()

        self.startCasting(globe.TRAINING_STR) 

        waitForAck()

        #globe.experiment_flow_global.remove0Tails()
        globe.multiProcQueue.put(globe.experiment_flow_global)


    def contPhase(self, phase):     # phase can be train/training no matter capitals, otherwise predict
        print("starting additional training")

        if(phase.lower().startswith("train")):      # accepts train / training / etc...
            self.clientsTraining()
            phase = globe.TRAINING_STR
        else:
            self.clientsPredict()
            phase = globe.PREDICTION_STR

        waitForAck()

        self.startCasting(phase) 

        waitForAck()

        globe.multiProcQueue.put(globe.experiment_flow_global)

    def predict(self):
        print('Prediction - Starting...')

        globe.experiment_flow_global.syncPredicitionWithFlow()

        self.clientsPredict()

        waitForAck()

        self.startCasting(globe.PREDICTION_STR)

        waitForAck()
        
        # globe.experiment_flow_global.remove0Tails()
        globe.multiProcQueue.put(globe.experiment_flow_global)

    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics')
        globe.pendingAcks = 1
        waitForAck()
    

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
