import requests
import globalVars as globe
import time

class Transmitter:

    def __init__(self, mainServerAddress):
        self.mainServerAddress = mainServerAddress
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
        print('Clients Training Phase')
        response = requests.post(self.clientsTrainingAddress, data='')
        if globe.jupyterFlag == 0:
            print(response.ok, response.status_code)
        
    def updateCSV(self, currentPhase : int, sourceName): # currentPhase is either "Training", "Prediction" or "Statistics". 
        print('Update CSV Phase')

        # Compose the correct string to send as data to the Main Server:
        workersUnderSourceList = globe.expFlow[currentPhase][sourceName]
        workersUnderSourceStr = ",".join(workersUnderSourceList)
        if currentPhase == "Training":
            dataStr = '{},{},heart2020Edit_train_splitted'.format(sourceName, workersUnderSourceStr) #Python's string format, {} are swapped by the variables in the brackets respectively.
        elif currentPhase == "Prediction":
            dataStr = '{},{},heart2020Edit_predict_splitted'.format(sourceName, workersUnderSourceStr) #Python's string format, {} are swapped by the variables in the brackets respectively.

        response = requests.post(self.updateCSVAddress, data=dataStr)

        if globe.jupyterFlag == 0:
            print(response.ok, response.status_code)

    def startCasting(self):
        print('Start Casting Phase')


        dataStr = "{},{}".format(globe.components.toString('s'), globe.components.batchSize) #Python's string format, {} are swapped by the variables in the brackets respectively.

        response = requests.post(self.startCastingAddress, data=dataStr)

        if globe.jupyterFlag == 0:
            print(response.ok, response.status_code)

    def clientsPredict(self):
        print('Clients Predict Phase')
        response = requests.post(self.clientsPredictAddress, data='')
        if globe.jupyterFlag == 0:
            print(response.ok, response.status_code)

    def train(self):
        print('\nTraining - Starting...')

        # 1 Ack for clientsTraining(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1 + len(globe.components.sources) 

        self.clientsTraining()

        for source in globe.components.sources:
            self.updateCSV("Training", source)

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 
        
        # 1 Ack for startCasting():
        globe.pendingAcks += 1
        self.startCasting()

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        globe.multiProcQueue.put(globe.lossMaps[-1])


    def predict(self):
        print('Prediction - Starting...')

        # 1 Ack for clientsPredict(), <num of sources> Acks for updateCSV():
        globe.pendingAcks += 1 + len(globe.components.sources) 

        self.clientsPredict()

        for source in globe.components.sources:
            self.updateCSV("Prediction", source)

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        # 1 Ack for startCasting():
        globe.pendingAcks += 1
        self.startCasting()

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        globe.multiProcQueue.put(globe.lossMaps[-1])

    def statistics(self):
        globe.pendingAcks += 1
        requests.post(self.statisticsAddress, data='getStatistics')
    
if __name__ == "__main__":
    trans = Transmitter()
    trans.clientsTraining()

    '''
    def ackTest(self):
        globe.pendingAcks += 1
        response = requests.get('http://127.0.0.1:8095' + '/testglobe')
        print(int(response.content))
    '''

    '''
    def testQueue(address):
        for i in range(0, 10):
            globe.q.put(testPost(address, i+1))

        empty = globe.q.empty()
    
        return empty
    
    def wait():
        while not globe.ackQueue.empty(): #While the queue is NOT empty
            pass
    '''
