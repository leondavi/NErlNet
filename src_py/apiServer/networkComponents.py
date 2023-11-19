################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################

import sys
from definitions import *

sys.path.insert(0, NERLNET_SRC_PY_PATH)
from nerlPlanner.JsonDistributedConfigDefs import *
from nerlPlanner.JsonElements import GetFields

#import globalVars as globe
API_SERVER_STR = GetFields.get_api_server_field_name()
MAIN_SERVER_STR = GetFields.get_main_server_field_name()

class NetworkComponents():

    def __init__(self, dc_json):
        # Loading the data in JSON format:
        self.jsonData = dc_json

        # Getting the desired batch size:
        self.batchSize = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_BATCH_SIZE])
        self.frequency = int(self.jsonData[KEY_NERLNET_SETTINGS][KEY_FREQUENCY])

        # Getting the address of the main server:
        self.mainServerIp, self.mainServerPort = self.get_api_server_ip_port() #TODO
        self.apiServerIp, self.apiServerPort = self.get_api_server_ip_port() #TODO

        # Getting the address for the receiver:
        self.receiverHost = self.jsonData[API_SERVER_STR]['host'] # TODO - find host from device of APISERVER
        self.receiverPort = self.jsonData[API_SERVER_STR]['port']

        # Initializing lists for all the relevant components:
        self.devicesIp = []
        self.clients = []
        self.workers = []
        self.federateds = []
        self.sources = []
        self.sourceMethods = []
        self.sourceEpochs = {}
        self.routers = []

        # Getting the names of all the devices:
        devicesJsons = self.jsonData["devices"]

        for device in devicesJsons:
            self.devicesIp.append(device[GetFields.get_ipv4_field_name()]) #TODO Guy - this is how you can get fields names of nerlplanner

        # Getting the names of all the clients and workers:
        clientsJsons = self.jsonData['clients']

        for client in clientsJsons:
            self.clients.append(client['name'])
            subWorkers = client['workers'].split(',')
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)

        # # Getting the names of all the federated components:
        # federatedsJsons = self.jsonData['federated']
        # for federated in federatedsJsons:
        #     self.federateds.append(federated['name'])

        # Getting the names of all the sources:
        sourcesJsons = self.jsonData['sources']
        for source in sourcesJsons:
            self.sources.append(source['name'])
            self.sourceMethods.append(source['method'])
            self.sourceEpochs[source['name']] = source['epochs']

        # Getting the names of all the routers:
        routersJsons = self.jsonData['routers']
        for router in routersJsons:
            self.routers.append(router['name'])


    def get_main_server_ip_port(self):
        mainServerJson = self.jsonData[MAIN_SERVER_STR]
        return "", 0
    
    def get_api_server_ip_port(self):
        return "", 0


    def printComponents(self):
        print(f"Network components:\n \
                Receiver's Address: http://{self.receiverHost}:{self.receiverPort}\n \
                Frequency: {self.frequency}\n \
                Batchsize: {self.batchSize}\n \
                devicesIp: {self.devicesIp}\n \
                mainServerIp: {self.mainServerIp}\n \
                mainServerPort: {self.mainServerPort}\n \
                Clients: {self.clients}\n \
                Workers: {self.workers}\n \
                Federated networks: {self.federateds}\n \
                Sources: {self.sources} (mode={self.sourceMethods})\n \
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
        elif char == 'f':
            return ','.join(self.federateds)
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
f - federateds\n \
s - sources\n \
r - routers')




