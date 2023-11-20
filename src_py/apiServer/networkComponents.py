################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################

import sys
from definitions import *

sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}/nerlPlanner')
sys.path.insert(0, f'{NERLNET_SRC_PY_PATH}') # keep both paths for vscode intelisense

from logger import *
from nerlPlanner.JsonDistributedConfigDefs import *
from nerlPlanner.JsonElements import GetFields

#import globalVars as globe
API_SERVER_STR = GetFields.get_api_server_field_name()
MAIN_SERVER_STR = GetFields.get_main_server_field_name()

class NetworkComponents():

    def __init__(self, dc_json):
        # Loading the data in JSON format:
        self.jsonData = dc_json

        # Initializing lists for all the relevant components:
        self.devicesIp = []
        self.clients = []
        self.workers = []
        self.sources = []
        self.sourcesPolicies = []
        self.sourceEpochs = {}
        self.routers = []

        # Initializing maps
        self.map_entity_to_device = {}
        self.map_device_to_ip = {}

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
        self.receiverHost = self.apiServerIp
        self.receiverPort =  self.apiServerPort

        # Getting the names of all the clients and workers:
        clientsJsons = self.jsonData[GetFields.get_clients_field_name()]

        for client in clientsJsons:
            self.clients.append(client[GetFields.get_name_field_name()])
            subWorkers = client[GetFields.get_workers_field_name()].split(',')
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)

        # Getting the names of all the sources:
        sourcesJsons = self.jsonData[GetFields.get_sources_field_name()]
        for source in sourcesJsons:
            self.sources.append(source[GetFields.get_name_field_name()])
            self.sourcesPolicies.append(source[GetFields.get_policy_field_name()])
            self.sourceEpochs[source[GetFields.get_name_field_name()]] = source[GetFields.get_epochs_field_name()]

        # Getting the names of all the routers:
        routersJsons = self.jsonData[GetFields.get_routers_field_name()]
        for router in routersJsons:
            self.routers.append(router[GetFields.get_name_field_name()])


    def get_main_server_ip_port(self):
        main_server_port = self.jsonData[MAIN_SERVER_STR][GetFields.get_port_field_name()]
        main_server_ip = self.map_device_to_ip[self.map_entity_to_device[MAIN_SERVER_STR]]
        return main_server_ip, main_server_port
    
    def get_api_server_ip_port(self):
        api_server_port = self.jsonData[API_SERVER_STR][GetFields.get_port_field_name()]
        api_server_ip = self.map_device_to_ip[self.map_entity_to_device[API_SERVER_STR]]
        return api_server_ip, api_server_port


    def printComponents(self):
        LOG_INFO(f"\nNetwork components:\n \
                Receiver's Address: http://{self.receiverHost}:{self.receiverPort}\n \
                Frequency: {self.frequency} [batches/sec]\n \
                Batchsize: {self.batchSize} [samples]\n \
                devicesIp: {self.devicesIp}\n \
                mainServerIp: {self.mainServerIp}\n \
                mainServerPort: {self.mainServerPort}\n \
                Clients: {self.clients}\n \
                Workers: {self.workers}\n \
                Sources: {self.sources}\n \
                Routers: {self.routers}")

    def checkIdenticalAdresses(self):
        addressesDict = {}

        for idx, item in enumerate(self.jsonData["serverAPI"]):
            name = f"API{str(idx)}"
            address = item["host"] + item["port"]
            if address in addressesDict.values():
                print("Two identical addresses. Please check architecture JSON.")
                addressesDict[name] = address
                return addressesDict
            else:
                addressesDict[name] = address

        for idx, item in enumerate(self.jsonData["mainServer"]):
            name = f"MainServer{str(idx)}"
            address = item["host"] + item["port"]
            if address in addressesDict.values():
                print("Two identical addresses. Please check architecture JSON.")
                addressesDict[name] = address
                return addressesDict
            else:
                addressesDict[name] = address
        
        for item in self.jsonData["routers"]:
            address = item["host"] + item["port"]
            if address in addressesDict.values():
                print("Two identical addresses. Please check architecture JSON.")
                addressesDict[name] = address
                return addressesDict
            else:
                addressesDict[name] = address

        for item in self.jsonData["clients"]:
            name = item["name"]
            address = item["port"]
            if address in addressesDict.values():
                print("Two identical addresses. Please check architecture JSON.")
                addressesDict[name] = address
                return addressesDict
            else:
                addressesDict[name] = address

        for item in self.jsonData["sources"]:
            name = item["name"]
            address = item["port"]
            if address in addressesDict.values():
                print("Two identical addresses. Please check architecture JSON.")
                addressesDict[name] = address
                return addressesDict
            else:
                addressesDict[name] = address

        print("No identical addresses were found in the architecture.")
        return addressesDict
         
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




