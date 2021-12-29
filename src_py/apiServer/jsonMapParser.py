import json

class networkMap():

    def __init__(self, path):
        # Loading the data in JSON format:
        file = open(path)
        self.jsonData = json.load(file)

        # Getting the IP address of the main server:
        mainServerJson = self.jsonData['mainServer'][0]
        self.mainServerIp = mainServerJson['host']

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

if __name__ == "__main__":
    path = 'src_py/apiServer/map.json'
    map = networkMap(path)
    print(map.routers)




