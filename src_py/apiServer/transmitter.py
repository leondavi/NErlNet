################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import requests
import globalVars as globe
import sys
import os
import json
import zlib
import time
from definitions import *
from logger import *
from experiment_flow import *
from requests.exceptions import ConnectionError as RequestsConnectionError
from requests.exceptions import Timeout as RequestsTimeout

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

    def _post_with_retry(
        self,
        url: str,
        *,
        data=None,
        files=None,
        timeout=None,
        operation: str = "request",
        retries: int = 20,
        retry_sleep_sec: float = 0.5
    ):
        last_exc = None
        for attempt in range(1, retries + 1):
            try:
                return requests.post(url, data=data, files=files, timeout=timeout)
            except (RequestsConnectionError, RequestsTimeout) as exc:
                last_exc = exc
                if attempt < retries:
                    LOG_WARNING(
                        f"{operation} transient failure ({attempt}/{retries}) for {url}: {exc}. "
                        f"retrying in {retry_sleep_sec:.2f}s"
                    )
                    time.sleep(retry_sleep_sec)
                else:
                    break
        LOG_ERROR(
            f"{operation} failed after {retries} attempt(s) for {url}: {last_exc}"
        )
        raise last_exc

    def send_ack_validation(self):
        try:
            response = self._post_with_retry(
                self.ack_validation_address,
                data="ok",
                operation="send_ack_validation",
                retries=8,
                retry_sleep_sec=0.25
            )
            if not response.ok:
                LOG_ERROR(f"Failed to send batch ack")
        except (ConnectionRefusedError, RequestsConnectionError):
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.ack_validation_address}")
            raise ConnectionRefusedError
        except (ConnectionError, RequestsTimeout):
            LOG_ERROR(f"Connection Error: failed to connect to {self.ack_validation_address}")
            raise ConnectionError

    def clients_set_phase(self, phase: str, parallel_execution = None): 
        LOG_INFO(f'Phase {phase} requested from Main Server')
        payload = phase
        if isinstance(parallel_execution, dict):
            mode = str(parallel_execution.get("mode", "legacy")).strip().lower()
            if mode != "legacy":
                payload = json.dumps({
                    "phase": phase,
                    "parallelExecution": parallel_execution
                })
        try:
            response = self._post_with_retry(
                self.clientsPhaseUpdateAddress,
                data=payload,
                operation="clients_set_phase",
                retries=20,
                retry_sleep_sec=0.5
            )
            if not response.ok:
                LOG_ERROR(f"Failed to update phase")
        except (ConnectionRefusedError, RequestsConnectionError):
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.clientsPhaseUpdateAddress}")
            raise ConnectionRefusedError
        except (ConnectionError, RequestsTimeout):
            LOG_ERROR(f"Connection Error: failed to connect to {self.clientsPhaseUpdateAddress}")
            raise ConnectionError

    
    def send_jsons_to_devices(self, files):
        try:
            response = self._post_with_retry(
                self.send_jsons_address,
                files=files,
                timeout=8,
                operation="send_jsons_to_devices",
                retries=20,
                retry_sleep_sec=0.5
            )
            if not response.ok:
                response_preview = ""
                try:
                    response_preview = response.text[:300]
                except Exception:
                    response_preview = "<unavailable>"
                raise RuntimeError(
                    f"send_jsons_to_devices failed with HTTP {response.status_code} "
                    f"from {self.send_jsons_address}: {response_preview}"
                )
        except (ConnectionRefusedError, RequestsConnectionError):
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.send_jsons_address}")
            raise ConnectionRefusedError
        except (ConnectionError, RequestsTimeout):
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
            data_str_encoded =  None
            with open(csv_file, 'r') as file:
                csvfile = file.read()
                data_str_encoded = (f'{index + 1}#{total_sources}#{source_name}#{target_workers}#{phase_type}#{num_of_batches}#{nerltensor_type}#{csvfile}').encode()
            data_zip = zlib.compress(data_str_encoded)
            data_str_encoded = None
            try:
                response = self._post_with_retry(
                    self.updateCSVAddress,
                    data=data_zip,
                    operation=f"update_csv[{source_name}]",
                    retries=40,
                    retry_sleep_sec=0.5
                )
                if not response.ok: # If Code =/= 200
                    LOG_ERROR(f"Failed to update {csv_file} to Main Server")
            except (ConnectionRefusedError, RequestsConnectionError):
                LOG_ERROR(f"Connection Refused Error: failed to connect to {self.updateCSVAddress}")
                raise ConnectionRefusedError
            except (ConnectionError, RequestsTimeout):
                LOG_ERROR(f"Connection Error: failed to connect to {self.updateCSVAddress}")
                raise ConnectionError
            LOG_INFO(f'{((index+1)/total_sources)*100:.2f}% Sent')
        LOG_INFO(f'Data Transmission To Sources Is Completed!')

    def start_casting(self, experiment_phase : ExperimentPhase):
        dataStr = f"{experiment_phase.get_sources_str_list()}" 
        try:
            response = self._post_with_retry(
                self.startCastingAddress,
                data=dataStr,
                operation="start_casting",
                retries=20,
                retry_sleep_sec=0.5
            ) #startCasting to sources
            if not response.ok:
                LOG_ERROR(f"Failed to start casting to sources")
        except (ConnectionRefusedError, RequestsConnectionError):
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.startCastingAddress}")
            raise ConnectionRefusedError
        except (ConnectionError, RequestsTimeout):
            LOG_ERROR(f"Connection Error: failed to connect to {self.startCastingAddress}")
            raise ConnectionError

    def restart(self):
        requests.post(self.restart_address, data='restart')

    def statistics(self, event_sync_inst : EventSync):
        LOG_INFO("Statistics requested from Main Server")
        event_sync_inst.set_event_wait(event_sync_inst.COMMUNICATION_STATS)
        try:
            response = self._post_with_retry(
                self.statisticsAddress,
                data='getStatistics',
                operation="statistics",
                retries=20,
                retry_sleep_sec=0.5
            ) 
            if not response.ok:
                LOG_ERROR(f"Failed to get statistics from Main Server")
        except (ConnectionRefusedError, RequestsConnectionError):
            LOG_ERROR(f"Connection Refused Error: failed to connect to {self.statisticsAddress}")
            raise ConnectionRefusedError
        except (ConnectionError, RequestsTimeout):
            LOG_ERROR(f"Connection Error: failed to connect to {self.statisticsAddress}")
            raise ConnectionError
        event_sync_inst.sync_on_event(event_sync_inst.COMMUNICATION_STATS)
        LOG_INFO("Statistics received from Main Server")

    def terminate_receiver(self, reciver_address : str, api_server_event_sync_inst : EventSync):
        requests.post(self.mainServerAddress + '/terminate', data='terminate') # Todo change to Api server address
