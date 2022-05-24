from multiprocessing import Process
from transmitter import Transmitter
import globalVars as globe
from receiver import *
import time
import requests
import threading

class ApiServer():
    def __init__(self): 
        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort

        # Send the content of jsonPath to each devices:
        print("Sending jsonPath to devices...\n")
        toSend = globe.content[0].replace('\n','')+'#'+globe.content[1].replace('\n','')
        print(toSend)
        for ip in globe.components.devicesIp:
        
            print(ip + ":8484/updateJsonPath"+toSend)
            address = 'http://{}:8484/updateJsonPath'.format(ip)
            response = requests.post(address, data=toSend)

            if globe.jupyterFlag == 0:
                print(response)

        time.sleep(3)
        
        # Starting receiver flask server process
        print("Starting the receiver HTTP server...\n")

        self.serverThread = threading.Thread(target=initReceiver, args=())
        self.serverThread.start()
        time.sleep(1)

        self.transmitter = Transmitter(self.mainServerAddress)

    def getWorkersList(self):
        return globe.components.toString('w')
    
    def getRoutersList(self):
        return globe.components.toString('r')
    
    def getSourcesList(self):
        return globe.components.toString('s')
        
    def getTransmitter(self):
        return self.transmitter

    def stopServer(self):
        receiver.stop()
        return True

    def getQueueData(self):
        received = False
        
        while not received:
            if not multiProcQueue.empty():
                print("~New loss map has been created successfully~")
                multiProcQueue.get()
                received = True
            time.sleep(0.1)
   
    def train(self):
        globe.lossMaps.append({})
        self.transmitter.train()
        self.getQueueData()
        if globe.jupyterFlag == 0:
            print(globe.lossMaps[-1])
        print('Training - Finished\n')
        return globe.lossMaps[-1]

    def predict(self):
        globe.lossMaps.append({})
        self.transmitter.predict()
        self.getQueueData()
        if globe.jupyterFlag == 0:
            print(globe.lossMaps[-1])
        print('Prediction - Finished\n')
        return globe.lossMaps[-1]
    
    def statistics(self):
        self.transmitter.statistics()

if __name__ == "__main__":
    apiServerInst = ApiServer()
    apiServerInst.train()
    #print(globe.lossMaps)
    apiServerInst.predict()
    #apiServerInst.statistics()
    #transmitterInst = apiServerInst.getTransmitter()
    #transmitterInst.testPost()

'''
 def exitHandler(self):
        print("\nServer shutting down")
        exitReq = requests.get(self.mainServerAddress + '/shutdown')
        if exitReq.ok:
            exit(0)
        else:
            print("Server shutdown failed")
            exit(1)
'''
'''
        #serverState = None

        #while(serverState != SERVER_DONE):
         #  if not serverState:
          #    if it is integer: 
           #         serverState = self.managerQueue.get()

                #TODO add timeout mechanism (if mainServer falls kill after X seconds)
            #sleep(5)
        # wait on Queue
        #return ack.json() 
'''
