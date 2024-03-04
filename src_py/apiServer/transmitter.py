################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import requests
import globalVars as globe
import time
import sys
import os
from definitions import *
from experiment import *
from logger import *

def waitForAck():
    while globe.pendingAcks > 0:
        time.sleep(0.005)

class Transmitter:

    def __init__(self, experiment : Experiment, mainServerAddress, input_data_path : str):
        # Addresses used throughout the module:
        self.experiment = experiment
        self.input_data_path = input_data_path
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
        LOG_INFO('Training Phase requested from Main Server')
        response = requests.post(self.clientsTrainingAddress, data='')
        if not response.ok:
            LOG_ERROR('Training Phase Request issue!')

    def clientsPredict(self):
        globe.pendingAcks = 1
        LOG_INFO('Prediction Phase requested from Main Server')
        response = requests.post(self.clientsPredictAddress, data='')
        if not response.ok:
            LOG_ERROR('Prediction Phase Request issue!')

    def updateCSV(self, phase): # currentPhase is either "Training", "Prediction" or "Statistics". 
        csvfile = None
        currentPhase = PHASE_STR_DICT[phase]
        #split data by sources and send to mainServer:
        for root, dirnames, filenames in os.walk(self.input_data_path):
            for filename in filenames:
                if filename == f"{globe.experiment_focused_on.expFlow['CSV path']}_{currentPhase.lower()}.csv":
                    LOG_INFO(f"Reading csv file: {root}/{filename} for phase {currentPhase}")
                    with open(os.path.join(root, filename), 'r') as file:
                        csvfile = file.read()
                    break

        SourceData = []
        linesPerSource = int(len(csvfile)/len(globe.components.sources))
        for row in range(0,len(csvfile),linesPerSource):
            SourceData.append(csvfile[row:row+linesPerSource])

        for i,source in enumerate(globe.experiment_focused_on.expFlow[currentPhase]): # Itterate over sources in accordance to current phase
            sourceName = source['source name']
            workersUnderSource = source['workers']

            try:    epochs = 1 if currentPhase == "Prediction" else globe.components.sourceEpochs[sourceName]
            except: epochs = 1

            dataStr = f'{sourceName}#{workersUnderSource}#{epochs}#{SourceData[i]}'

            response = requests.post(self.updateCSVAddress, data=dataStr)

        LOG_INFO("Data sent to sources")

        globe.sourceCSVIndex=linesPerSource # TODO find what is the purpose of this line? and where using it

    def startCasting(self, phase):
        print('\nStart Casting Phase')

        # 1 Ack for startCasting():
        globe.pendingAcks = 1 

        # numOfBatches, is no. of batches to request from the Main Server. On the other side, Batch size is found at the architecture JSOn, which is available at globe.components
        if (phase==globe.TRAINING_STR):
            batchesPerSource = globe.experiment_focused_on.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.TRAINING_STR]
        elif (phase==globe.PREDICTION_STR):
            batchesPerSource = globe.experiment_focused_on.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.PREDICTION_STR]
        else:
            batchesPerSource = sys.maxsize

        dataStr = f"{globe.components.toString('s')},{batchesPerSource}" #sources, batches

        response = requests.post(self.startCastingAddress, data=dataStr) #startCasting to sources

        if globe.jupyterFlag == False:
            print(response.ok, response.status_code)

    def train(self):
        self.experiment.syncTrainingWithFlow()
        self.clientsTraining()
        waitForAck()
        self.startCasting(globe.TRAINING_STR) 
        waitForAck()
        return self.experiment.name

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

    def predict(self):
        self.experiment.syncPredicitionWithFlow()
        self.clientsPredict()
        waitForAck()
        self.startCasting(globe.PREDICTION_STR)
        waitForAck()
        return self.experiment.name

    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics')
        globe.pendingAcks = 1
        waitForAck()
        
    def restart(self):
        print("Sent restart request to Main Server!")
        requests.post(self.restart_address, data='restart')
        time.sleep(5)

    

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
