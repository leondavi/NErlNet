from transmitter import Transmitter
import globalVars as globe
import receiver
import time
import requests
import threading
import matplotlib.pyplot as plt
import pandas as pd
import sys
import numpy as np
import os

class ApiServer():
    def __init__(self):        
        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort
        self.experiments = []
        
        print("Initializing the receiver thread...\n")

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        print("Using the address from the architecture JSON file for the receiver.")
        print(f"(http://{globe.components.receiverHost}:{globe.components.receiverPort})\n")

        self.receiverProblem = threading.Event()
        print(self.receiverProblem)
        self.receiverThread = threading.Thread(target = receiver.initReceiver, args = (globe.components.receiverHost, globe.components.receiverPort, self.receiverProblem), daemon = True)
        self.receiverThread.start()   
        self.receiverThread.join(2) # After 2 secs, the receiver is either running, or the self.receiverProblem event is set.

        if (self.receiverProblem.is_set()): # If a problem has occured when trying to run the receiver.
            print("Failed to initialize the receiver using the provided address.\n\
Please change the 'host' and 'port' values for the 'serverAPI' key in the architecture JSON file.\n")
            sys.exit()      

        # Initalize an instance for the transmitter: 
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
            if globe.jupyterFlag == False:
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

    # Wait for a result to arrive to the queue, and get results which arrived:
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
        print("\nPlease choose a name for the current experiment:", end = ' ')
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
        # Create a new folder for the results:
        if not os.path.exists('/usr/local/lib/nerlnet-lib/NErlNet/Results'):
            os.mkdir('/usr/local/lib/nerlnet-lib/NErlNet/Results')

        if (len(self.experiments) == 0):
            print("No experiments were conducted yet.")
            return

        print("\n---STATISTICS---\n")
        print("List of saved experiments:")
        for i, exp in enumerate(self.experiments, start=1): 
            print(f"{i}) {exp.name}")

        while True:
            print("\nPlease choose an experiment number:", end = ' ')
            expNum = input()

            # Add exception for non-numeric inputs:
            try:
                expNum = int(expNum)
            except ValueError:
                print("\nIllegal Input") 
                continue

            if (expNum > 0 and expNum <= len(self.experiments)):
                expForStats = self.experiments[expNum-1]
                break
            
            # Continue with the loop if expNum is not in the list:
            else:
                print("\nIllegal Input")

        print("\n---Statistics Menu---\n\
1) Create plot for the training loss function.\n\
2) Display prediction accuracy.\n\
3) Export results to CSV.\n")

        while True:
            print("\nPlease choose an option:", end = ' ')
            option = input()

            try:
                option = int(option)
            except ValueError:
                print("\nIllegal Input") 
                continue

            if (option > 0 and option <= 3):
                break

            else:
                print("\nIllegal Input") 
        
        if (option == 1):
            numOfCsvs = len(expForStats.trainingResList)

            print(f"\nThe training phase contains {numOfCsvs} CSVs:")
            for i, csvRes in enumerate(expForStats.trainingResList, start=1):
                print(f"{i}) {csvRes.name}")

            while True:
                print("\nPlease choose a CSV number for the plot:", end = ' ')       
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

            plt.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/results/Exp:{expForStats.name}_CSV:{csvResPlot.name}.png')
            plt.show()

            return

        # A vizualiztion for correct/wrong samples:
        if (option == 2):
            print("\nPlease prepare a CSV with the last column containing the samples' labels.")

            while True:
                print("\nPlease enter the NON-SPLITTED CSV's path:", end = ' ') 
                print("/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/", end = '')      
                labelsCsvPath = input()
                labelsCsvPath = '/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/' + labelsCsvPath

                try:
                    csvDf = pd.read_csv(labelsCsvPath)
                    break

                except OSError:
                    print("\nInvalid path\n")

            # Extract the labels (last) column from the CSV. Create a list of labels:
            labelsDf = csvDf.iloc[:,-1]
            labels = pd.unique(labelsDf)

            # Choose the matching (to the original labeled CSV) CSV from the prediction results list:
            numOfCsvs = len(expForStats.predictionResList)

            print(f"\nThe prediction phase contains {numOfCsvs} CSVs:")
            for i, csvRes in enumerate(expForStats.predictionResList, start=1):
                print(f"{i}) {csvRes.name}")

                while True:
                    print("\nPlease choose the number for the corresponding (matching) CSV result:", end = ' ')       
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

            accDict = {} # For each sample: 1/0 if the prediction was right/wrong. 
            

            workersPredictions = csvResAcc.workersResList

            # Generate the samples' indexes from the results:
            for worker in workersPredictions:
                for batch in worker.resList:
                    for offset, prediction in enumerate(batch.predictions):
                        sampleNum = batch.indexRange[0] + offset
                        normsDict = {} #  For all labels: label : the "distance" of prediction from the label. 

                        # The distances of the current prediction from each of the labels: 
                        for i, label in enumerate(labels):
                            newNorm = abs(prediction - label)
                            normsDict[label] = newNorm

                        # If there is minimum distance from the correct label - 1. Otherwise - 0:
                        nearestLabel = min(normsDict, key=normsDict.get)

                        if (nearestLabel == labelsDf.iloc[sampleNum]):
                            accDict[sampleNum] = 1

                        else:
                            accDict[sampleNum] = 0
            
            # Calculate the accuracy:
            correctPreds = sum(accDict.values())
            accuracy = correctPreds / len(accDict)

            '''
            powIdx = 1
            while True:
                size = powIdx**2
                nextSize = ((powIdx+1)**2)

                if (nextSize > len(accDict)):
                    break
                
                powIdx += 1

            accDictPreds = list(accDict.values())
            accDictPreds = np.array(accDictPreds)
            accDictPreds = accDictPreds[:size]
            accDictPreds = accDictPreds.reshape((powIdx,powIdx))

            print(accDictPreds)
            expTitle = (expForStats.name)

            plt.figure(figsize = (8,8), dpi = 150)
            plt.imshow(accDictPreds, cmap = 'OrRd')

            plt.title(f"Prediction - Accuracy - {expTitle}", fontsize = 24)

            for (i,j), _ in np.ndenumerate(accDictPreds):
                txt = f'{(i * powIdx) + j + 1}'
                plt.text(i, j, txt, ha='center', va='center', fontsize = 4, fontweight='bold')

            plt.tick_params(left = False, right = False , labelleft = False , labelbottom = False, bottom = False)
            plt.colorbar()

            plt.show()
            '''
    
            print(f"\nAccuracy acquired: {round(accuracy, 3)} ({round(accuracy*100, 3)}%).")
            return accuracy

        if (option == 3):
            # Create a new folder for the CSVs of the chosen experiment:
            if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}'):
                os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}')

            numOfTrainingCsvs = len(expForStats.trainingResList)
            numOfPredicitionCsvs = len(expForStats.predictionResList)

            print(f"\nThe training phase contains {numOfTrainingCsvs} CSVs:")
            for i, csvTrainRes in enumerate(expForStats.trainingResList, start=1):
                print(f"{i}) {csvTrainRes.name}")

            for csvTrainRes in expForStats.trainingResList:
                # Create a new folder for the train results:
                if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training'):
                    os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training')

                workersResCsv = csvTrainRes.workersResList.copy() # Craete a copy of the results list for the current CSV.

                for i in range(len(workersResCsv)):
                    workersResCsv[i] = pd.Series(workersResCsv[i].resList, name = workersResCsv[i].name, index = None)
                    
                newCsvDf = pd.concat(workersResCsv, axis=1)
                print(newCsvDf)

                fileName = csvTrainRes.name.rsplit('/', 1)[1] # If th eCSV name contains a path, then take everything to the right of the last '/'.
                newCsvDf.to_csv(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training/{fileName}.csv', header = True, index = False)
            '''
            print(f"\nThe prediction phase contains {numOfPredicitionCsvs} CSVs:")
            for i, csvPredictionRes in enumerate(expForStats.predictionResList, start=1):
                print(f"{i}) {csvPredictionRes.name}")

            # Generate the samples' indexes from the results:
            for worker in workersPredictions:
                for batch in worker.resList:
                    for offset, prediction in enumerate(batch.predictions):
                        sampleNum = batch.indexRange[0] + offset
            '''

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
