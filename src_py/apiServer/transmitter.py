################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import requests
import globalVars as globe
import sys
import os
import zlib
from definitions import *
from logger import *
from experiment_flow import *

class Transmitter:

    def __init__(self, experiment_flow : ExperimentFlow, mainServerAddress):
        # Addresses used throughout the module:
        self.experiment_flow = experiment_flow
        self.mainServerAddress = mainServerAddress
        self.sourceInitAddr = self.mainServerAddress + '/sourceInit'
        self.clientsPhaseUpdateAddress = self.mainServerAddress + '/clientsPhaseUpdate'
        self.updateCSVAddress = self.mainServerAddress + '/updateCSV'
        self.startCastingAddress = self.mainServerAddress + '/startCasting'
        self.statisticsAddress = self.mainServerAddress + '/statistics'
        self.restart_address = self.mainServerAddress + '/restart'
        self.ack_validation_address = self.mainServerAddress + '/apiserver_ack_validation'
        main_server_http_with_init_port = f'{self.mainServerAddress.split(":")[0]}:{self.mainServerAddress.split(":")[1]}:{JSON_INIT_HANDLER_ERL_PORT}'
        self.send_jsons_address = main_server_http_with_init_port + '/sendJsons'

    def send_ack_validation(self):
        try:
            response = requests.post(self.ack_validation_address, data = "ok")
            if not response.ok:
                LOG_ERROR(f"Failed to send batch ack")
        except ConnectionRefusedError:
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.ack_validation_address}")
            raise ConnectionRefusedError
        except ConnectionError:
            LOG_ERROR(f"Connection Error: failed to connect to {self.ack_validation_address}")
            raise ConnectionError

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
        total_sources = len(csv_files)
        assert total_sources == len(source_pieces)
        for index in range(total_sources):
            csv_file = csv_files[index]
            source_piece = source_pieces[index]
            source_name = source_piece.get_source_name()
            target_workers = source_piece.get_target_workers()
            num_of_batches = source_piece.get_num_of_batches()
            nerltensor_type = source_piece.get_nerltensor_type()
            phase_type = source_piece.get_phase()
            with open(csv_file, 'r') as file:
                csvfile = file.read()
                data_str = f'{index + 1}#{total_sources}#{source_name}#{target_workers}#{phase_type}#{num_of_batches}#{nerltensor_type}#{csvfile}'
                data_zip = zlib.compress(data_str.encode())
                try:
                    response = requests.post(self.updateCSVAddress, data = data_zip)
                    if not response.ok: # If Code =/= 200
                        LOG_ERROR(f"Failed to update {csv_file} to Main Server")
                except ConnectionRefusedError: 
                    LOG_ERROR(f"Connection Refused Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionRefusedError
                except ConnectionError:
                    LOG_ERROR(f"Connection Error: failed to connect to {self.updateCSVAddress}")
                    raise ConnectionError
            LOG_INFO(f'{((index+1)/total_sources)*100:.2f}% Sent')
        LOG_INFO(f'Data Transmission To Sources Is Completed!')

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