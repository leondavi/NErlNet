import requests
import globalVars as globe
import time

class Transmitter:

    def __init__(self):
        self.mainServerAddress = globe.mainServerAddress
        self.clientsTrainingAddress = self.mainServerAddress + '/clientsTraining'
        self.updateCSVAddress = self.mainServerAddress + '/updateCSV'
        self.startCastingAddress = self.mainServerAddress + '/startCasting'
        self.clientsPredictAddress = self.mainServerAddress + '/clientsPredict'
        self.statisticsAddress = self.mainServerAddress + '/statistics'

    def testPost(self,address,payloadNum):
        payload = {'test' : payloadNum}
        response = requests.post(address ,data = payload)
        print(response.ok, response.status_code, response.json())
        #Return true, if received: HTTP status code < 400
        #Return the HTTP status code for the response
        #Return the reponse in JSON format
        return(response.ok, response.status_code, response.json())

    def clientsTraining(self):
        print('Training - Clients Training Phase')

        response = requests.post(self.clientsTrainingAddress, data='')
        print(response.ok, response.status_code)
        
    def updateCSV(self):
        print('Training - Update CSV Phase')

        response = requests.post(self.updateCSVAddress, data='s1,w1,RunOrWalkTrain_splitted')
        print(response.ok, response.status_code)

    def startCasting(self):
        print('Training - Start Casting  Phase')

        response = requests.post(self.startCastingAddress, data='s1,100')
        print(response.ok, response.status_code)

    def clientsPredict(self):
        response = requests.post(self.clientsPredictAddress, data='')
        print(response.ok, response.status_code)

    def train(self):
        print('Training - Starting...')

        globe.pendingAcks += 3 #TODO: Remove magic number, pay atention to global variable changes.

        self.clientsTraining()
        self.updateCSV()

        while globe.pendingAcks > 1:
            time.sleep(0.005)
            pass 

        self.startCasting()

    def predict(self):
        print('Prediction - Starting...')

        #globe.pendingAcks += 3 #TODO: Remove magic number, pay atention to global variable changes.

        self.clientsPredict()
        self.updateCSV()

        while globe.pendingAcks > 1:
            time.sleep(0.005)
            pass 

        self.startCasting()

    def statistics(self):
        requests.post(self.statisticsAddress, data='getStatistics')
    
    def ackTest(self):
        globe.pendingAcks += 1
        response = requests.get('http://127.0.0.1:8095' + '/testglobe')
        print(int(response.content))

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
    trans = Transmitter()
    trans.clientsTraining()
#ins = Transmitter()
#ins.train()