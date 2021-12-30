from multiprocessing import Process
from transmitter import Transmitter
import globalVars as globe
from receiver import *
import time
import requests
import threading


class ApiServer():
    def __init__(self): 
        self.mainServerIP = 'http://192.168.0.102'
        self.mainServerPort = '8080'
        self.mainServerAddress = mainServerIP + ':' + mainServerPort
        
        # Starting receiver flask server process
        self.serverThread = threading.Thread(target=initReceiver, args=())
        self.serverThread.start()
        time.sleep(1)

        self.transmitter = Transmitter(self.mainServerAddress)

    def exitHandler(self):
        print("\nServer shutting down")
        exitReq = requests.get(self.mainServerAddress + '/shutdown')
        if exitReq.ok:
            exit(0)
        else:
            print("Server shutdown failed")
            exit(1)

    def getTransmitter(self):
        return self.transmitter

    def stopServer(self):
        receiver.stop()
        return True

    def getQueueData(self):
        received = False
        
        while not received:
            if not multiProcQueue.empty():
                print("Loss map created!")
                lossMap = multiProcQueue.get()
                print(lossMap)
                return lossMap      
            time.sleep(0.1)
   
    def train(self):
        self.transmitter.train()
        self.getQueueData()
        
    def statistics(self):
        self.transmitter.statistics()

        #serverState = None

        #while(serverState != SERVER_DONE):
         #  if not serverState:
          #    if it is integer: 
           #         serverState = self.managerQueue.get()

                #TODO add timeout mechanism (if mainServer falls kill after X seconds)
            #sleep(5)
        # wait on Queue
        #return ack.json() 

if __name__ == "__main__":
    apiServerInst = ApiServer()
    #apiServerInst.transmitter.statistics()
    apiServerInst.train()
    #transmitterInst = apiServerInst.getTransmitter()
    #transmitterInst.testPost()
