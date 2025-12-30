################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################

import sys
import os
import hashlib
from definitions import *

sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}/nerlPlanner')
sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}') # keep both paths for vscode intelisense

from logger import *
from nerlPlanner.JsonDistributedConfigDefs import *
from nerlPlanner.JsonElements import GetFields

#import globalVars as globe
API_SERVER_STR = GetFields.get_api_server_field_name()
MAIN_SERVER_STR = GetFields.get_main_server_field_name()

# types
TYPE_CLIENT = "client"
TYPE_WORKER = "worker"
TYPE_SOURCE = "source"
TYPE_ROUTER = "router"
TYPE_WORKER = "worker"
TYPE_MAIN_SERVER = "mainServer"

class NetworkComponents():

    def __init__(self, dc_json: dict):
        # Loading the data in JSON format:
        self.jsonData = dc_json

        # Initializing lists for all the relevant components names:
        self.devicesIp = []
        self.clients = []
        self.workers = []
        self.sources = []
        self.sourcesPolicies = []
        self.sourceEpochs = {}
        self.routers = []
        self.sources_policy_dict = {}
        

        # Initializing maps
        self.map_worker_to_client = {}
        self.map_entity_to_device = {}
        self.map_device_to_ip = {}
        self.map_name_to_type = {}
        self.worker_to_model_sha = {}
        self.model_sha_map = self.jsonData.get(KEY_MODEL_SHA, {})
        self.torch_model_assets = {}

        # Getting the desired batch size:
        self.batchSize = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE])
        self.frequency = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_FREQUENCY])

        # Getting the names of all the devices:
        devicesJsons = self.jsonData[GetFields.get_devices_field_name()]

        for device in devicesJsons:
            self.devicesIp.append(device[GetFields.get_ipv4_field_name()])
            for entity_name in device[GetFields.get_entities_field_name()].split(','):
                self.map_entity_to_device[entity_name] = device[GetFields.get_name_field_name()]
                self.map_device_to_ip[device[GetFields.get_name_field_name()]] = device[GetFields.get_ipv4_field_name()]

        # Getting the address of the main server:
        self.mainServerIp, self.mainServerPort = self.get_main_server_ip_port()
        self.apiServerIp, self.apiServerPort = self.get_api_server_ip_port()

        # Getting the address for the receiver:
        self.receiverIp = self.apiServerIp
        self.receiverPort =  self.apiServerPort

        # Getting the names of all the clients and workers:
        clientsJsons = self.jsonData[GetFields.get_clients_field_name()]

        for client_dict in clientsJsons:
            self.clients.append(client_dict[GetFields.get_name_field_name()])
            subWorkers = client_dict[GetFields.get_workers_field_name()].split(',') # list
            for worker_name in subWorkers:
                self.map_worker_to_client[worker_name] = client_dict[GetFields.get_name_field_name()] # map worker name to client name
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)
            self.map_name_to_type[client_dict[GetFields.get_name_field_name()]] = TYPE_CLIENT

        workers_section = self.jsonData.get(GetFields.get_workers_field_name(), [])
        for worker_def in workers_section:
            worker_name = worker_def.get(GetFields.get_name_field_name())
            worker_sha = worker_def.get(WORKER_MODEL_SHA_FIELD)
            if worker_name and worker_sha:
                self.worker_to_model_sha[worker_name] = worker_sha

        # Getting the names of all the sources:
        sourcesJsons = self.jsonData[GetFields.get_sources_field_name()]
        for source in sourcesJsons:
            self.sources.append(source[GetFields.get_name_field_name()])
            self.sourcesPolicies.append(source[GetFields.get_policy_field_name()])
            self.sourceEpochs[source[GetFields.get_name_field_name()]] = source[GetFields.get_epochs_field_name()]
            self.map_name_to_type[source[GetFields.get_name_field_name()]] = TYPE_SOURCE
            self.sources_policy_dict[source[GetFields.get_name_field_name()]] = source[GetFields.get_policy_field_name()]

        # Getting the names of all the routers:
        routersJsons = self.jsonData[GetFields.get_routers_field_name()]
        for router in routersJsons:
            self.routers.append(router[GetFields.get_name_field_name()])
            self.map_name_to_type[router[GetFields.get_name_field_name()]] = TYPE_ROUTER

        self.torch_model_assets = self._extract_torch_assets()


    def get_map_worker_to_client(self):
        return self.map_worker_to_client
    
    def get_source_epochs_dict(self):
        return self.sourceEpochs
    
    def get_client_name_by_worker_name(self, worker_name):
        return self.map_worker_to_client[worker_name]

    def get_main_server_ip_port(self):
        main_server_port = self.jsonData[MAIN_SERVER_STR][GetFields.get_port_field_name()]
        main_server_ip = self.map_device_to_ip[self.map_entity_to_device[MAIN_SERVER_STR]]
        return main_server_ip, main_server_port
    
    def get_api_server_ip_port(self):
        api_server_port = self.jsonData[API_SERVER_STR][GetFields.get_port_field_name()]
        api_server_ip = self.map_device_to_ip[self.map_entity_to_device[API_SERVER_STR]]
        return api_server_ip, api_server_port
    
    def get_freq(self):
        return self.frequency
    
    def get_batch_size(self):
        return self.batchSize
    
    def get_num_of_sources(self):
        return len(self.sources)
    
    def get_workers_list(self):
        return self.workers

    def printComponents(self):
        LOG_INFO(f"\nNetwork components:\n \
                Receiver's Address: http://{self.receiverIp}:{self.receiverPort}\n \
                Frequency: {self.frequency} [batches/sec]\n \
                Batchsize: {self.batchSize} [samples]\n \
                devicesIp: {self.devicesIp}\n \
                mainServerIp: {self.mainServerIp}\n \
                mainServerPort: {self.mainServerPort}\n \
                apiServerIp: {self.apiServerIp}\n \
                apiServerPort: {self.apiServerPort}\n \
                Clients: {self.clients}\n \
                Workers: {self.workers}\n \
                Sources: {self.sources}\n \
                Routers: {self.routers}")

    def has_torch_models(self) -> bool:
        return bool(self.torch_model_assets)

    def get_torch_model_assets(self):
        return dict(self.torch_model_assets)

    def _extract_torch_assets(self):
        assets = {}
        if not self.model_sha_map:
            return assets

        for model_sha, model_payload in self.model_sha_map.items():
            infra_type = str(model_payload.get('infraType', '')).lower()
            if infra_type != 'torch':
                continue

            pt_path = model_payload.get('pt_path')
            if not pt_path:
                raise ValueError(f"Torch model {model_sha} is missing 'pt_path' in distributed config")

            resolved_path = self._resolve_asset_path(pt_path)
            if not os.path.isfile(resolved_path):
                raise FileNotFoundError(f"Torch model file not found for {model_sha}: {resolved_path}")

            remote_path = build_torch_remote_model_path(model_sha, os.path.basename(resolved_path))
            actual_checksum = self._calculate_sha256(resolved_path)
            expected_checksum = model_payload.get('pt_checksum')
            if expected_checksum and expected_checksum.lower() not in ('placeholder', 'none'):
                if actual_checksum.lower() != expected_checksum.lower():
                    raise ValueError(f"Checksum mismatch for Torch model {model_sha}")

            assets[model_sha] = {
                'sha': model_sha,
                'local_path': resolved_path,
                'remote_path': remote_path,
                'format': model_payload.get('pt_format', 'torchscript'),
                'description': model_payload.get('pt_description', ''),
                'checksum': actual_checksum,
            }

        return assets

    def _resolve_asset_path(self, asset_path: str) -> str:
        normalized = asset_path.strip()
        candidate_paths = []
        if os.path.isabs(normalized):
            candidate_paths.append(normalized)
        else:
            candidate_paths.append(os.path.abspath(normalized))
            candidate_paths.append(os.path.abspath(os.path.join(NERLNET_PATH, normalized)))

        for candidate in candidate_paths:
            if os.path.isfile(candidate):
                return candidate

        raise FileNotFoundError(f"Unable to resolve Torch model path: {asset_path}")

    def _calculate_sha256(self, file_path: str) -> str:
        sha256_hash = hashlib.sha256()
        with open(file_path, 'rb') as file_obj:
            for chunk in iter(lambda: file_obj.read(1024 * 1024), b''):
                sha256_hash.update(chunk)
        return sha256_hash.hexdigest()

         
    def toString(self, char): #Prints the contents of any of the components' lists (e.g. "routers")
        if char == 'd':
            return ','.join(self.devicesIp)
        elif char == 'c':
            return ','.join(self.clients)
        elif char == 'w':
            return ','.join(self.workers)
        elif char == 's':
            return ','.join(self.sources)
        elif char == 'r':
            return ','.join(self.routers)
        else:
            raise ValueError('Not a valid char!\n \
Please enter a valid char as input:\n \
d - devices Ip\n \
c - clients\n \
w - workers\n \
s - sources\n \
r - routers')




