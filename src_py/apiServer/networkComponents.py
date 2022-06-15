import json
import globalVars as globe

class NetworkComponents():

    def __init__(self, path):
        # Loading the data in JSON format:
        file = open(path)
        self.jsonData = json.load(file)

        # Getting the desired batch size:
        self.batchSize = int(self.jsonData['NerlNetSettings'][0]['batchSize'])
        self.frequency = int(self.jsonData['NerlNetSettings'][0]['frequency'])
        # Getting IP address of the main server:
        mainServerJson = self.jsonData['mainServer'][0]
        self.mainServerIp = mainServerJson['host']
        self.mainServerPort = mainServerJson['port']

        # Initializing lists for all the relevant components:
        self.devicesIp = []
        self.clients = []
        self.workers = []
        self.federateds = []
        self.sources = []
        self.routers = []

        # Getting the names of all the devices:
        devicesJsons = self.jsonData["devices"]

        for device in devicesJsons:
            self.devicesIp.append(device["host"])

        # Getting the names of all the clients and workers:
        clientsJsons = self.jsonData['clients']

        for client in clientsJsons:
            self.clients.append(client['name'])
            subWorkers = client['workers'].split(',')
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)

        # Getting the names of all the federated components:
        federatedsJsons = self.jsonData['federated']
        for federated in federatedsJsons:
            self.federateds.append(federated['name'])

        # Getting the names of all the sources:
        sourcesJsons = self.jsonData['sources']
        for source in sourcesJsons:
            self.sources.append(source['name'])

        # Getting the names of all the routers:
        routersJsons = self.jsonData['routers']
        for router in routersJsons:
            self.routers.append(router['name'])

    def printComponents(self):
        print(f"Network components:\n \
                Batchsize: {self.batchSize}\n \
                Frequency: {self.frequency}\n \
                devicesIp: {self.devicesIp}\n \
                mainServerIp: {self.mainServerIp}\n \
                mainServerPort: {self.mainServerPort}\n \
                Clients: {self.clients}\n \
                Workers: {self.workers}\n \
                Federated networks: {self.federateds}\n \
                Sources: {self.sources}\n \
                Routers: {self.routers}\n")

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

if __name__ == "__main__":
    path = globe.componentsPath
    map = NetworkComponents(path)
    map.printComponents()
    print(map.toString('s'))
    print(map.checkIdenticalAdresses())
    #print(map.toString('k'))




