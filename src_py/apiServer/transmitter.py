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
        self.restart_address = self.mainServerAddress + '/restart'
        main_server_http_with_init_port = f'{self.mainServerAddress.split(":")[0]}:{self.mainServerAddress.split(":")[1]}:{JSON_INIT_HANDLER_ERL_PORT}'
        self.send_jsons_address = main_server_http_with_init_port + '/sendJsons'
        


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
        try:
            response = requests.post(self.clientsPhaseUpdateAddress, data = phase)
            if not response.ok:
                LOG_ERROR(f"Failed to update phase")
        except ConnectionRefusedError:
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.clientsPhaseUpdateAddress}")
            raise ConnectionRefusedError
        except ConnectionError:
            LOG_ERROR(f"Connection Error: failed to connect to {self.clientsPhaseUpdateAddress}")
            raise ConnectionError

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
    
    def send_jsons_to_devices(self, files):
        try:
            response = requests.post(self.send_jsons_address, files = files, timeout = 8)
            if not response.ok:
                LOG_ERROR(f"Failed to send json files to Main Server")
        except ConnectionRefusedError:
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.send_jsons_address}")
            raise ConnectionRefusedError
        except ConnectionError:
            LOG_ERROR(f"Connection Error: failed to connect to {self.send_jsons_address}")
            raise ConnectionError
        

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
                except ConnectionRefusedError: 
                    LOG_ERROR(f"Connection Refused Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionRefusedError
                except ConnectionError:
                    LOG_ERROR(f"Connection Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionError

    def start_casting(self, experiment_phase : ExperimentPhase):
        dataStr = f"{experiment_phase.get_sources_str_list()}" 
        try:
            response = requests.post(self.startCastingAddress, data=dataStr) #startCasting to sources
            if not response.ok:
                LOG_ERROR(f"Failed to start casting to sources")
        except ConnectionRefusedError:
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.startCastingAddress}")
            raise ConnectionRefusedError
        except ConnectionError:
            LOG_ERROR(f"Connection Error: failed to connect to {self.startCastingAddress}")
            raise ConnectionError

    def restart(self):
        requests.post(self.restart_address, data='restart')

    def statistics(self, event_sync_inst : EventSync):
        LOG_INFO("Statistics requested from Main Server")
        event_sync_inst.set_event_wait(event_sync_inst.COMMUNICATION_STATS)
        try:
            response = requests.post(self.statisticsAddress, data='getStatistics') 
            if not response.ok:
                LOG_ERROR(f"Failed to get statistics from Main Server")
        except ConnectionRefusedError:
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.statisticsAddress}")
            raise ConnectionRefusedError
        except ConnectionError:
            LOG_ERROR(f"Connection Error: failed to connect to {self.statisticsAddress}")
            raise ConnectionError
        event_sync_inst.sync_on_event(event_sync_inst.COMMUNICATION_STATS)
        LOG_INFO("Statistics received from Main Server")

    def terminate_receiver(self, reciver_address : str, api_server_event_sync_inst : EventSync):
        requests.post(self.mainServerAddress + '/terminate', data='terminate') # Todo change to Api server address