###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay
import time
import requests
import threading
import matplotlib.pyplot as plt
import pandas as pd
import sys
import numpy as np
import os

from jsonDirParser import JsonDirParser
from transmitter import Transmitter
from networkComponents import NetworkComponents
import globalVars as globe
import receiver

class ApiServer():
    def __init__(self):       
        self.json_dir_parser = JsonDirParser()
        pass
    
    def initialization(self, arch_json: str, conn_map_json, experiment_flow_json):
        archData = self.json_dir_parser.json_from_path(arch_json)
        connData = self.json_dir_parser.json_from_path(conn_map_json)
        expData = self.json_dir_parser.json_from_path(experiment_flow_json)
        
        globe.experiment_flow_global.set_experiment_flow(expData)
        globe.components = NetworkComponents(archData) # TODO components path should come from jsonDirParser
        globe.components.printComponents()

        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort
        self.experiments = []
        
        print("Initializing the receiver thread...\n")

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        print("Using the address from the architecture JSON file for the receiver.")
        print(f"(http://{globe.components.receiverHost}:{globe.components.receiverPort})\n")

        self.receiverProblem = threading.Event()
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

        # Jsons found in NErlNet/inputJsonFiles/{JSON_TYPE}/files.... for entities in src_erl/Comm_layer/http_nerl/src to reach them, they must go up 3 dirs
        archAddress , connMapAddress, exp_flow_json = self.getUserJsons()
        #[JsonsPath, archPath] = archAddress.split("/NErlNet")
        #archAddress = "../../.."+archPath

        #[JsonsPath, connPath] = connMapAddress.split("/NErlNet")
        #connMapAddress = "../../.."+connPath

        data = archAddress + '#' + connMapAddress

        for ip in globe.components.devicesIp:
            with open(archAddress, 'rb') as f1, open(connMapAddress, 'rb') as f2:
                files = [('arch.json', f1), ('conn.json', f2)]
                address = f'http://{ip}:8484/updateJsonPath' # f for format
                response = requests.post(address, files=files)

            # response = requests.post(address, data, timeout = 10)
            if globe.jupyterFlag == False:
              print(response.ok, response.status_code)

        #split experiment data and send to individual sources:

        time.sleep(1)
        print("JSON paths sent to devices")

    def showJsons(self):
        self.json_dir_parser.print_lists()
    
    def selectJsons(self):
        self.json_dir_parser.select_arch_connmap_experiment()

    def setJsons(self, arch, conn, exp):
        self.json_dir_parser.set_arch_connmap_experiment(arch, conn, exp)
    
    def getUserJsons(self):
        return self.json_dir_parser.get_user_selection_files()

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
        globe.experiment_flow_global.name = input()

        globe.experiment_flow_global.emptyExp() # Start a new empty experiment
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

        # Create a new folder for the CSVs of the chosen experiment:
        if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}'):
            os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}')

        print("\n---Statistics Menu---\n\
1) Create plot for the training loss function.\n\
2) Calculate accuracy and plot a confusion matrix.\n\
3) Export results to CSV.\n\
4) Display communication statistics.\n")

        while True:
            print("\nPlease choose an option:", end = ' ')
            option = input()

            try:
                option = int(option)
            except ValueError:
                print("\nIllegal Input") 
                continue

            if (option > 0 and option <= 4):
                break

            else:
                print("\nIllegal Input") 
        
        if (option == 1):
            # Create a new folder for to save an image of the plot:
            if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training'):
                os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training')

            numOfCsvs = len(expForStats.trainingResList)

            print(f"\nThe training phase contains {numOfCsvs} CSVs:")
            for i, csvRes in enumerate(expForStats.trainingResList, start=1):
                print(f"{i}) {csvRes.name}")

            while True:
                print("\nPlease choose a CSV number for the plot (for multiple CSVs, seperate their numbers with ', '):", end = ' ')       
                csvNumsStr = input()

                try:
                    csvNumsList = csvNumsStr.split(', ')
                    csvNumsList = [int(csvNum) for csvNum in csvNumsList]
                except ValueError:
                    print("\nIllegal Input") 
                    continue
                
                if (all(csvNum > 0 and csvNum <= numOfCsvs for csvNum in csvNumsList)): # Check if all CSV indexes are in the correct range.
                    break

                else:
                    print("\nInvalid Input") 

            # Draw the plot using Matplotlib:
            plt.figure(figsize = (30,15), dpi = 150)
            plt.rcParams.update({'font.size': 22})

            for csvNum in csvNumsList:
                csvResPlot = expForStats.trainingResList[csvNum-1]
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

                #fileName = csvResPlot.name.rsplit('/', 1)[1] # If th eCSV name contains a path, then take everything to the right of the last '/'.
                fileName = globe.experiment_flow_global.name
                plt.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training/{fileName}.png')
                print(f'\n{fileName}.png was Saved...')

            plt.show()

            return

        if (option == 2):
            if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction'):
                os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction')

            # Choose the matching (to the original labeled CSV) CSV from the prediction results list:
            numOfCsvs = len(expForStats.predictionResList)

            print(f"\nThe prediction phase contains {numOfCsvs} CSVs:")
            for i, csvRes in enumerate(expForStats.predictionResList, start=1):
                print(f"{i}) {csvRes.name}")

                while True:
                    print("\nPlease choose a CSV number for accuracy calculation and confusion matrix (for multiple CSVs, seperate their numbers with ', '):", end = ' ')       
                    csvNumsStr = input()

                    try:
                        csvNumsList = csvNumsStr.split(', ')
                        csvNumsList = [int(csvNum) for csvNum in csvNumsList]

                    except ValueError:
                        print("\nIllegal Input") 
                        continue       

                    if (all(csvNum > 0 and csvNum <= numOfCsvs for csvNum in csvNumsList)): # Check if all CSV indexes are in the correct range.
                        break

                    else:
                        print("\nInvalid Input") 

            print("\nPlease prepare the original labeled CSV, with the last column containing the samples' labels.")

            while True:
                print("\nPlease enter the path for the NON-SPLITTED labels CSV (including .csv):", end = ' ') 
                print("/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/", end = '')      
                labelsCsvPath = input()
                labelsCsvPath = '/usr/local/lib/nerlnet-lib/NErlNet/inputDataDir/' + labelsCsvPath

                try:
                    labelsCsvDf = pd.read_csv(labelsCsvPath)
                    break

                except OSError:
                    print("\nInvalid path\n")

            # Extract the labels (last) column from the CSV. Create a list of labels:
            labelsSeries = labelsCsvDf.iloc[:,-1]

            # If we are running an AEC - convert the 2 labels to 1's and 0's. (Majority (90%) label -> 1, Minority (10%) label -> 0).
            if (globe.components.aec == 1):
                labelsOccuranceSeries = labelsSeries.value_counts()
                maxOccuranceLabel = labelsOccuranceSeries.idxmax()
                labelsSeries = (labelsSeries == maxOccuranceLabel).astype(int)
                
            labelsArr = pd.unique(labelsSeries)
            labelsArr = np.sort(labelsArr) 

            predsDict = {} # A dictionary containing all the predictions
            accDict = {} # For each sample: 1/0 if the prediction was right/wrong. 
            
            for csvNum in csvNumsList:
                csvResAcc = expForStats.predictionResList[csvNum-1]

                workersPredictions = csvResAcc.workersResList

                # Generate the samples' indexes from the results:
                for worker in workersPredictions:
                    for batch in worker.resList:
                        for offset, prediction in enumerate(batch.predictions):
                            sampleNum = batch.indexRange[0] + offset
                            normsDict = {} #  The "distance" of the current prediction prediction from each of the labels. 

                            # The distances of the current prediction from each of the labels: 
                            for i, label in enumerate(labelsArr):
                                newNorm = abs(prediction - label)
                                normsDict[label] = newNorm

                            # If there is minimum distance from the correct label - 1. Otherwise - 0:
                            currentPrediction = min(normsDict, key=normsDict.get)

                            predsDict[sampleNum] = currentPrediction

                            if (currentPrediction == labelsSeries.iloc[sampleNum]):
                                accDict[sampleNum] = 1

                            else:
                                accDict[sampleNum] = 0
            
            # Calculate the accuracy:
            correctPreds = sum(accDict.values())
            accuracy = correctPreds / len(accDict)
            print(f"\nAccuracy acquired: {round(accuracy, 3)} ({round(accuracy*100, 3)}%).")

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

            predsSeries = pd.Series(predsDict)
            SlicedLabelsSeries = labelsSeries.iloc[:predsSeries.size]

            # Create a confusion matrix based on the results:
            SlicedLabelsSeriesStr = SlicedLabelsSeries.astype(str) # Convert floats to string, because confusion cannot handle "continous" values
            predsSeriesStr = predsSeries.astype(str) # Convert floats to string, because confusion cannot handle "continous" values
            labelsArrStr = labelsArr.astype(str) # Convert floats to string, because confusion cannot handle "continous" values
            # Another option to solve this problem, is to numerize each classification group (group 1, group 2, ...), 
            # and add legened to show the true label value for each group.
            confMat = confusion_matrix(SlicedLabelsSeriesStr, predsSeriesStr, labels = labelsArrStr)
            confMatDisp = ConfusionMatrixDisplay(confMat, display_labels = labelsArr)
            fig, ax = plt.subplots(figsize = (10,10), dpi = 150)
            plt.rcParams.update({'font.size': 14})
            expTitle = expForStats.name
            ax.set_title(f"Prediction - Confusion Matrix - {expTitle}\nAccuracy: {round(accuracy, 3)} ({round(accuracy*100, 3)}%)", fontsize = 18)
            ax.set_xlabel('True Labels', fontsize = 16)
            ax.set_ylabel('Predicted Labels', fontsize = 16)
            confMatDisp.plot(ax = ax)
            plt.show()

            fileName = csvResAcc.name.rsplit('/', 1)[1] # If the CSV name contains a path, then take everything to the right of the last '/'.
            confMatDisp.figure_.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction/{fileName}.png')
            print(f'\n{fileName}.png Saved...')

            return accuracy, confMat

        if (option == 3):
            # Create a new folder for the train results:
            if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training'):
                os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training')

             # Create a new folder for the prediction results:
            if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction'):
                os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction')

            numOfTrainingCsvs = len(expForStats.trainingResList)
            numOfPredicitionCsvs = len(expForStats.predictionResList)

            print(f"\nCreating training results files for the following {numOfTrainingCsvs} CSVs:")
            for i, csvTrainRes in enumerate(expForStats.trainingResList, start=1):
                print(f"{i}) {csvTrainRes.name}")
            print('\n')

            for csvTrainRes in expForStats.trainingResList:
                workersTrainResCsv = csvTrainRes.workersResList.copy() # Craete a copy of the results list for the current CSV.

                for i in range(len(workersTrainResCsv)):
                    workersTrainResCsv[i] = pd.Series(workersTrainResCsv[i].resList, name = workersTrainResCsv[i].name, index = None)
                    
                newlabelsCsvDf = pd.concat(workersTrainResCsv, axis=1)

                fileName = csvTrainRes.name.rsplit('/', 1)[1] # If th eCSV name contains a path, then take everything to the right of the last '/'.
                newlabelsCsvDf.to_csv(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training/{fileName}.csv', header = True, index = False)
                print(f'{fileName}.csv Saved...')
            
            print(f"\nCreating prediction results files for the following {numOfPredicitionCsvs} CSVs:")
            for i, csvPredictionRes in enumerate(expForStats.predictionResList, start=1):
                print(f"{i}) {csvPredictionRes.name}")
            print('\n')

            for csvPredictRes in expForStats.predictionResList:
                csvPredictResDict = {} # Dictionary of sampleIndex : [worker, batchId]

                # Add the results to the dictionary
                for worker in csvPredictRes.workersResList:
                    for batch in worker.resList:
                        for offset, prediction in enumerate(batch.predictions):
                            sampleNum = batch.indexRange[0] + offset
                            csvPredictResDict[sampleNum] = [prediction, batch.worker, batch.batchId]

                csvPredictResDf = pd.DataFrame.from_dict(csvPredictResDict, orient='index', columns = ['Prediction', 'Handled By Worker', 'Batch ID'])
                csvPredictResDf.index.name = 'Sample Index'

                fileName = csvPredictRes.name.rsplit('/', 1)[1] # If th eCSV name contains a path, then take everything to the right of the last '/'.
                csvPredictResDf.to_csv(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction/{fileName}.csv', header = True, index = True)
                print(f'{fileName}.csv Saved...')

                return

        if (option == 4):
            self.transmitter.statistics()

                
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