################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import requests
import globalVars as globe
import sys
import os
from definitions import *
from experiment import * #deprecated
from logger import *
from experiment_flow import *

class Transmitter:

    def __init__(self, experiment : Experiment, mainServerAddress, input_data_path : str):
        # Addresses used throughout the module:
        self.experiment = experiment
        self.input_data_path = input_data_path
        self.mainServerAddress = mainServerAddress
        self.sourceInitAddr = self.mainServerAddress + '/sourceInit'
        self.clientsTrainingAddress = self.mainServerAddress + '/clientsTraining'  #deprecated
        self.clientsPhaseUpdateAddress = self.mainServerAddress + '/clientsPhaseUpdate'
        self.updateCSVAddress = self.mainServerAddress + '/updateCSV'
        self.startCastingAddress = self.mainServerAddress + '/startCasting'
        self.clientsPredictAddress = self.mainServerAddress + '/clientsPredict' #deprecated
        self.statisticsAddress = self.mainServerAddress + '/statistics'

    def testPost(self, address, payloadNum):
        payload = {'test' : payloadNum}
        response = requests.post(address ,data = payload)
        print(response.ok, response.status_code, response.json())
        #Return true, if received: HTTP status code < 400
        #Return the HTTP status code for the response
        #Return the reponse in JSON format
        return(response.ok, response.status_code, response.json())

    def clients_set_phase(self, phase: str): 
        LOG_INFO(f'Phase {phase} requested from Main Server')
        response = requests.post(self.clientsPhaseUpdateAddress, data = phase)
        # Todo try / catch 
        if not response.ok:
            LOG_ERROR(f'Phase {phase} Request issue!')

    def clientsTraining(self):   #deprecated
        globe.set_receiver_wait_for_ack()
        LOG_INFO('Training Phase requested from Main Server')
        response = requests.post(self.clientsTrainingAddress, data='')
        if not response.ok:
            LOG_ERROR('Training Phase Request issue!')

    def clientsPredict(self):    #deprecated
        globe.set_receiver_wait_for_ack()
        LOG_INFO('Prediction Phase requested from Main Server')
        response = requests.post(self.clientsPredictAddress, data='')
        if not response.ok:
            LOG_ERROR('Prediction Phase Request issue!')

    def update_csv(self, csv_files: list, source_pieces: list):
        assert len(csv_files) == len(source_pieces)
        for index in range(len(csv_files)):
            csv_file = csv_files[index]
            source_piece = source_pieces[index]
            source_name = source_piece.get_source_name()
            target_workers = source_piece.get_target_workers()
            num_of_batches = source_piece.get_num_of_batches()
            with open(csv_file, 'r') as file:
                csvfile = file.read()
                data_str = f'{source_name}#{target_workers}#{num_of_batches}#{csvfile}'
                try:
                    response = requests.post(self.updateCSVAddress, data = data_str)
                    if not response.ok:
                        LOG_ERROR(f"Failed to update {csv_file} to Main Server")
                except ConnectionError:
                    LOG_ERROR(f"Connection Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionError
                except ConnectionRefusedError: 
                    # Todo - check if this is the right way to handle this
                    LOG_ERROR(f"Connection Refused Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionRefusedError

    def start_casting(self, experiment_pase:ExperimentPhase):
        dataStr = f"{experiment_pase.get_sources_str_list()}" # Todo Guy please support this pattern
        requests.post(self.startCastingAddress, data=dataStr) #startCasting to sources

    def startCasting(self, phase):
        # numOfBatches, is no. of batches to request from the Main Server. On the other side, Batch size is found at the architecture JSOn, which is available at globe.components
        if (phase==globe.TRAINING_STR):
            batchesPerSource = globe.experiment_focused_on.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.TRAINING_STR]
        elif (phase==globe.PREDICTION_STR):
            batchesPerSource = globe.experiment_focused_on.expFlow[globe.BATHCHES_PER_SOURCE_STR][globe.PREDICTION_STR]
        else:
            batchesPerSource = sys.maxsize

        # TODO - sources don't always start with 's'
        dataStr = f"{globe.components.toString('s')},{batchesPerSource}" # Todo check with Guy

        globe.set_receiver_wait_for_ack()
        globe.ack_debug_print()
        requests.post(self.startCastingAddress, data=dataStr) #startCasting to sources
        globe.ack_debug_print()


    def train(self):
        self.experiment.syncTrainingWithFlow()
        self.clientsTraining() # set receiver wait for ack
        globe.ack_debug_print()
        globe.waitForAck()
        globe.ack_debug_print()
        self.startCasting(globe.TRAINING_STR) # set receiver wait for ack
        globe.ack_debug_print()
        globe.waitForAck()
        globe.ack_debug_print()
        return self.experiment.name

    def contPhase(self, phase):     # phase can be train/training no matter capitals, otherwise predict
        print("starting additional training")

        if(phase.lower().startswith("train")):      # accepts train / training / etc...
            self.clientsTraining()
            phase = globe.TRAINING_STR
        else:
            self.clientsPredict()
            phase = globe.PREDICTION_STR

        globe.waitForAck()
        self.startCasting(phase) 
        globe.waitForAck()

    def predict(self):
        LOG_INFO("Predict phase starts")
        globe.ack_debug_print()
        self.experiment.syncPredicitionWithFlow()
        self.clientsPredict() # set receiver wait for ack
        globe.ack_debug_print()
        globe.waitForAck()
        globe.ack_debug_print()
        self.startCasting(globe.PREDICTION_STR)
        globe.ack_debug_print()
        globe.waitForAck()
        globe.ack_debug_print()
        return self.experiment.name

    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics') # ! Continue from here
        globe.pendingAcks = 1
        globe.waitForAck()
    

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
