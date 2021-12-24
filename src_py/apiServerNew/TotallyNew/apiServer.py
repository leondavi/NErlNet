from multiprocessing import Process
from transmitter import Transmitter
import globalVars as globe
from receiver import *
import time
import requests
import threading


class ApiServer():
    def __init__(self): 
        self.mainServerAddress = globe.mainServerAddress
        # starting receiver flask server process

        newThread = threading.Thread(target=initReceiver, args=())
        self.threads = [] # list of processes
        self.threads.append(newThread)
        newThread.start()
        self.getQueueData()
        time.sleep(1)
        self.transmitter = Transmitter()

        #self.manager = globe.manager
        #self.managerQueue = globe.managerQueue

    def exitHandler(self, signum, frame):
            exitReq = requests.get(self.mainServerAddress + '/shutdown')
            if exitReq.ok:
                print("\nServer shutting down")
                exit(0)

    def getTransmitter(self):
        return self.transmitter

    def stopServer(self):
        receiver.stop()
        return True

    def getQueueData(self):
        print("Starting receiver server...")
        received = False
        while not received:
            if not multiProcQueue.empty():
                print("Message received")
                msg = multiProcQueue.get()
                print("message: "+str(msg))
                received = True
            time.sleep(1)

        print("Stopping server")
        #self.stopServer()

    def train(self, mainServerAddress, batchSize):
        self.transmitter.train()

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
    apiServerInst.transmitter.train()
    #transmitterInst = apiServerInst.getTransmitter()
    #transmitterInst.testPost()
