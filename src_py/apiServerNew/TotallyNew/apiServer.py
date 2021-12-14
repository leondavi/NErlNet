from multiprocessing import Process, Manager
from transmitter import Transmitter
import globalVars as globe
import receiver
import os
import time

class ApiServer():
    def __init__(self): 
        self.mainServerAddress = globe.mainServerAddress

        self.transmitter = Transmitter()
        self.runReceiver()

        self.manager = globe.manager
        self.managerQueue = globe.managerQueue

    def getTransmitter(self):
        return self.transmitter

    def runReceiver(self):
        print("Starting receiver server...")

        #Creating a seperate process for the receiver, in the following block:
        procs = []

        p1 = Process(target=receiver.initReceiver, args=())
        procs.append(p1)

        p1.start()
        #It takes some time for the server to fully initiate
        time.sleep(0.5)

        print("Receiver server started!")    

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
