from multiprocessing import Manager
from transmitter import Transmitter
from receiver import *

class APIServer():
    def __init__(self,mainServerIP = '127.0.0.1', mainServerPort = '8080'): 
        self.mainServerAddress = self.mainServerIP +":"+ self.mainServerPort
        self.transmitter = Transmitter(self.mainServerAddress)
        self.runReceiver()
        self.manager = manager
        self.managerQueue = managerQueue

    def getTransmitter(self):
        return self.transmitter

    def runReceiver(self):
        print("Starting receiver server...")
        receiver.run(debug=True, threaded=True, port=8095)

    def train(self, mainServerAddress, batchSize):
        self.transmitter.initTrain()
        if (not "200OK") # TODO fix
            throw exception

        serverState = None
        while(serverState != SERVER_DONE):
            if not serverState:
                if it is integer: 
                    serverState = self.managerQueue.get()

                #TODO add timeout mechanism (if mainServer falls kill after X seconds)
            sleep(5)
        # wait on Queue
        return dataLabels 
        

if __name__ == "__main__":
    apiServerInst = APIServer()
    #transmitterInst = apiServerInst.getTransmitter()
    #transmitterInst.testPost()
