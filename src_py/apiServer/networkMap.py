import json

class NetworkMap():

    def __init__(self, path):
        # Loading the data in JSON format:
        file = open(path)
        self.jsonData = json.load(file)


        # Getting the desired batch size:
        self.batchSize = int(self.jsonData['NerlNetSettings'][0]['batchSize'])

        # Getting IP address of the main server:
        mainServerJson = self.jsonData['mainServer'][0]
        self.mainServerIp = mainServerJson['host']
        self.mainServerPort = mainServerJson['port']

        # Getting the names of all the clients and workers:
        self.clients = []
        self.workers = []

        clientsJsons = self.jsonData['clients']

        for client in clientsJsons:
            self.clients.append(client['name'])
            subWorkers = client['workers'].split(',')
            # Add every sub-worker of this client, to the general workers list:
            self.workers.extend(subWorkers)

        # Getting the names of all the federated components:
        self.federateds = []

        federatedsJsons = self.jsonData['federated']
        for federated in federatedsJsons:
            self.federateds.append(federated['name'])

        # Getting the names of all the sources:
        self.sources = []

        sourcesJsons = self.jsonData['sources']
        for source in sourcesJsons:
            self.sources.append(source['name'])

        # Getting the names of all the routers:
        self.routers = []

        routersJsons = self.jsonData['routers']
        for router in routersJsons:
            self.routers.append(router['name'])

    def printMap(self):
        print(self.batchSize, self.mainServerIp, self.mainServerPort, self.clients, self.workers, self.federateds, self.sources, self.routers)

    def toString(self, char): 
        if char == 'c':
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
c - clients\n \
w - workers\n \
f - federateds\n \
s - sources\n \
r - routers')

if __name__ == "__main__":

    # path = 'map.json'

    path = '1s1wMap.json'

    map = NetworkMap(path)
    map.printMap()
    print(map.toString('s'))
    #print(map.toString('k'))




