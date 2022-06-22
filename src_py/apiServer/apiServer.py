from multiprocessing import Process
from transmitter import Transmitter
import globalVars as globe
from receiver import *
import time
import requests
import threading
import matplotlib.pyplot as plt
import pandas as pd

class ApiServer():
    def __init__(self): 
        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort
        self.experiments = []
        
        # Starting receiver flask server process:
        print("Starting the receiver HTTP server...\n")

        self.serverThread = threading.Thread(target=initReceiver, args=())
        self.serverThread.start()
        time.sleep(1)

        self.transmitter = Transmitter(self.mainServerAddress)

        print("\n***Please remember to execute NerlnetRun.sh before continuing.")
        
    def sendJsonsToDevices(self):
        # Send the content of jsonPath to each devices:
        print("\nSending JSON paths to devices...")

        archAddress = globe.content[0][:-1]
        connMapAddress = globe.content[1][:-1]
        data = archAddress + '#' + connMapAddress

        for ip in globe.components.devicesIp:
            address = f'http://{ip}:8484/updateJsonPath' # f for format

            response = requests.post(address, data, timeout = 10)
            if globe.jupyterFlag == 0:
              print(response.ok, response.status_code)

        time.sleep(1)
        print("JSON paths sent to devices")

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
            if not globe.multiProcQueue.empty():
                print("~New result has been created successfully~")
                expResults = globe.multiProcQueue.get() # Get the new result out of the queue
                received = True
            time.sleep(0.1)

        return expResults
   
    def train(self):
        # Choose a nem for the current experiment:
        print("\nPlease choose a name for the current experiment:")
        globe.expResults.name = input()

        globe.expResults.emptyExp() # Empty previous results
        self.transmitter.train()
        expResults = self.getQueueData()
        print('Training - Finished\n')
        return expResults

    def predict(self):
        self.transmitter.predict()
        expResults = self.getQueueData()
        print('Prediction - Finished\n')
        self.experiments.append(expResults) # Assuming a cycle of training -> prediction, saving only now.
        print("Experiment saved")
        return expResults
    
    def statistics(self):
        if (len(self.experiments) == 0):
            print("No experiments were condcted yet.")
            return

        print("STATISTICS\n")
        print("List of saved experiments:")
        for i, exp in enumerate(self.experiments, start=1): 
            print(f"{i}) {exp.name}")

        while True:
            print("\nPlease choose an experiment number:")
            expNum = input()

            try:
                expNum = int(expNum)
            except ValueError:
                print("\nIllegal Input") 
                continue

            if (expNum > 0 and expNum <= len(self.experiments)):
                expForStats = self.experiments[expNum-1]
                break

            else:
                print("\nIllegal Input")

        print("\n---Statistics Menu---\n\
1) Create plot for the training loss function.\n\
2) Display prediction accuracy.\n\
3) Export results to CSV.\n")

        while True:
            print("\nPlease choose an option:")
            option = input()

            try:
                option = int(option)
            except ValueError:
                print("\nIllegal Input") 
                continue

            if (option > 0 and option <= 2):
                break

            else:
                print("\nIllegal Input") 
        
        if (option == 1):
            numOfCsvs = len(expForStats.trainingResList)

            print(f"\nThe training phase contains {numOfCsvs} CSVs:")
            for i, csvRes in enumerate(expForStats.trainingResList, start=1):
                print(f"{i}) {csvRes.name}")

            while True:
                print("\nPlease choose a CSV number for the plot:")       
                csvNum = input()

                try:
                    csvNum = int(csvNum)
                except ValueError:
                    print("\nIllegal Input") 
                    continue

                if (csvNum > 0 and csvNum <= numOfCsvs):
                    csvResPlot = expForStats.trainingResList[csvNum-1]
                    break

                else:
                    print("\nIllegal Input") 

            # Draw the plot using Matplotlib:
            plt.figure(figsize = (30,15), dpi = 150)
            plt.rcParams.update({'font.size': 22})

            for workerRes in csvResPlot.workersResList:
                data = workerRes.resList
                plt.plot(data, linewidth = 3)

            expTitle = (expForStats.name)
            plt.title(f"Training - Loss Function - {expTitle}", fontsize=38)
            plt.xlabel('Batch No.', fontsize = 30)
            plt.ylabel('Loss (MSE)', fontsize = 30)
            plt.xlim(left=0)
            plt.ylim(bottom=0)
            plt.legend(csvResPlot.workers)
            plt.grid(visible=True, which='major', linestyle='-')
            plt.minorticks_on()
            plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)

            plt.show()

            return

        if (option == 2):
            print("\nPlease prepare a CSV with the last column containing the samples' labels.")

            while True:
                print("\nPlease enter the NON-SPLITTED CSV's path:") 
                print("/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/", end = '')      
                labelsCsvPath = input()
                labelsCsvPath = '/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/' + labelsCsvPath

                try:
                    csvDf = pd.read_csv(labelsCsvPath)

                except OSError:
                    print("\nInvalid path\n")

                labelsDf = csvDf.iloc[:,-1]
                labels = pd.unique(labelsDf)

                numOfCsvs = len(expForStats.predictionResList)

                print(f"\nThe prediction phase contains {numOfCsvs} CSVs:")
                for i, csvRes in enumerate(expForStats.predictionResList, start=1):
                    print(f"{i}) {csvRes.name}")

                while True:
                    print("\nPlease choose a number for the matching CSV result:")       
                    csvNum = input()

                    try:
                        csvNum = int(csvNum)
                    except ValueError:
                        print("\nIllegal Input") 
                        continue

                    if (csvNum > 0 and csvNum <= numOfCsvs):
                        csvResAcc = expForStats.predictionResList[csvNum-1]
                        break

                    else:
                        print("\nIllegal Input") 

                accDict = {}
                normsDict = {}

                workersPredictions = csvResAcc.workersResList

                for worker in workersPredictions:
                    for batch in worker.resList:
                        for offset, prediction in enumerate(batch.predictions):
                            sampleNum = batch.indexRange[0] + offset

                            for i, label in enumerate(labels):
                                newNorm = abs(prediction - label)
                                normsDict[label] = newNorm

                            nearestLabel = min(normsDict, key=normsDict.get)

                            if (nearestLabel == labelsDf.iloc[sampleNum]):
                                accDict[sampleNum] = 1

                            else:
                                accDict[sampleNum] = 0
                
                correctPreds = sum(accDict.values())
                accuracy = correctPreds / len(accDict)

                print(accDict)

                print(f"\n Accuracy calculated: {accuracy} ({accuracy*100})%.")
                return accuracy


        if (option == 3):
            #TODO
            pass

if __name__ == "__main__":
    apiServerInst = ApiServer()
    apiServerInst.sendJsonsToDevices()
    apiServerInst.train()
    apiServerInst.predict()
    apiServerInst.statistics()


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
