import requests
import globalVars2 as globe
import apiServer
#import time

#DEFAULT_PORT = 8095

class Transmitter():

    def __init__(self):
        self.mainServerAddress = apiServer.mainServerAddress
        self.clientsTrainingAddress = self.mainServerAddress + '/clientsTraining'
        self.updateCSVAddress = self.mainServerAddress + '/updateCSV'
        self.startCastingAddress = self.mainServerAddress + '/startCasting'
        self.clientsTrainingAddress = self.mainServerAddress + '/clientsPredict'
        self.statisticsAddress = self.mainServerAddress + '/statistics'

    def testPost(self,payloadNum):
        payload = {'test' : payloadNum}
        response = requests.post(self.mainServerAddress ,data = payload)
        print(response.ok, response.status_code, response.json())
        #Return true, if received: HTTP status code < 400
        #Return the HTTP status code for the response
        #Return the reponse in JSON format
        return(response.ok, response.status_code, response.json())

    def clientsTraining(self):
        requests.post(self.clientsTrainingAddress, data='')

    def updateCSV(self):
        requests.post(self.updateCSVAddress,
            data='Sources, Workers, RunOrWalkTrain_splitted')

    def startCasting(self):
        requests.post(self.startCastingAddress, 
            data='Sources, Workers, RunOrWalkPredictNolabels_splitted')

    def clientsPredict(self):
        requests.post(self.clientsPredictAddress, data='')
    
    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics')

    """
    def testQueue(address):
        for i in range(0, 10):
            globe.q.put(testPost(address, i+1))

        empty = globe.q.empty()
    
        return empty
    
    def wait():
        while not globe.ackQueue.empty(): #While the queue is NOT empty
            pass
    """

    if __name__ == "__main__":
        print('transmitter')