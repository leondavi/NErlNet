###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay, accuracy_score, classification_report
import time
import requests
import threading
import matplotlib.pyplot as plt
import pandas as pd
import sys
import numpy as np
import os
import subprocess

from jsonDirParser import JsonDirParser
from transmitter import Transmitter
from networkComponents import NetworkComponents
import globalVars as globe
import receiver

class ApiServer():
    def __init__(self):       
        self.json_dir_parser = JsonDirParser()

        # Create a new folder for the results:
        if not os.path.exists('/usr/local/lib/nerlnet-lib/NErlNet/Results'):
            os.mkdir('/usr/local/lib/nerlnet-lib/NErlNet/Results')

        pass

    def help(self):

    #i) data saved as .csv, training file ends with "_Training.csv", prediction with "_Prediction.csv" (may change in future)
        print(
"""
__________NERLNET CHECKLIST__________
0. Run this Jupyter in the folder of generated .py files!
1. Make sure data and jsons in correct folder, and jsons include the correct paths
    * Data includes: labeled prediction csv, training file, prediction file
    * Prediction CSVs need to be ordered the same!
            
____________API COMMANDS_____________
==========Setting experiment========

-showJsons():                       shows available arch / conn / exp layouts
-selectJsons():                     get input from user for arch / conn / exp selection
-setJsons(arch, conn, exp):         set layout in code
-getUserJsons():                    returns the selected arch / conn / exp
-initialization(arch, conn, exp):   set up server for a NerlNet run
-sendJsonsToDevices():              send each NerlNet device the arch / conn jsons to init entities on it
-sendDataToSources(phase):          phase can be "training" / "prediction". send the experiment data to sources (currently happens in beggining of train/predict)

========Running experiment==========
-train():                           start training phase
-predict():                         start prediction phase
-contPhase(phase):                  send another `Batch_size` of a phase (must be called after initial train/predict)

==========Experiment info===========
-print_saved_experiments()          prints saved experiments and their number for statistics later
-plot_loss(ExpNum)                  saves and shows the loss over time of the chosen experiment
-accuracy_matrix(ExpNum)            shows a graphic for the confusion matrix. Also returns all ConfMat[worker][nueron]
-communication_stats()              prints the communication statistics of the current network.
-statistics():                      *DEPRECATED* get specific statistics of experiment (lossFunc graph, accuracy, etc...)

_____GLOBAL VARIABLES / CONSTANTS_____
pendingAcks:                        makes sure API command reaches all relevant entities (wait for pending acks)
multiProcQueue:                     a queue for combining and returning data to main thread after train / predict phases
TRAINING_STR = "Training"
PREDICTION_STR = "Prediction"
        """)
    
    def initialization(self, arch_json: str, conn_map_json, experiment_flow_json):
        archData = self.json_dir_parser.json_from_path(arch_json)
        connData = self.json_dir_parser.json_from_path(conn_map_json)
        expData = self.json_dir_parser.json_from_path(experiment_flow_json)
        
        globe.experiment_flow_global.set_experiment_flow(expData)
        globe.components = NetworkComponents(archData)
        globe.components.printComponents()
        globe.experiment_flow_global.printExp()

        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort
        self.experiments = []
        
        print("Initializing the receiver thread...\n")

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        print("Using the address from the architecture JSON file for the receiver.")
        print(f"(http://{globe.components.receiverHost}:{globe.components.receiverPort})\n")


        receiverAlive = subprocess.run(["lsof", f"-i -P -n | grep -wc {globe.components.receiverPort}"], stdout=subprocess.PIPE)
        assert not receiverAlive.stdout, f"Reciever port {globe.components.receiverPort} is already being used"

        self.receiverProblem = threading.Event()
        self.receiverThread = threading.Thread(target = receiver.initReceiver, args = (globe.components.receiverHost, globe.components.receiverPort, self.receiverProblem), daemon = True)
        self.receiverThread.start()   
        self.receiverThread.join(2) # After 2 secs, the receiver is either running, or the self.receiverProblem event is set.

        if (self.receiverProblem.is_set()): # If a problem has occured when trying to run the receiver.
            print("Failed to initialize the receiver using the provided address.\n\
Please change the 'host' and 'port' values for the 'serverAPI' key in the architecture JSON file.\n")
            sys.exit()

        # Initalize an instance for the transmitter:
        if not hasattr(self, 'transmitter'):
            self.transmitter = Transmitter(self.mainServerAddress)

        print("\n***Please remember to execute NerlnetRun.sh on each device before continuing.")

    def sendJsonsToDevices(self):
        # Send the content of jsonPath to each devices:
        print("\nSending JSON paths to devices...")

        # Jsons found in NErlNet/inputJsonFiles/{JSON_TYPE}/files.... for entities in src_erl/Comm_layer/http_nerl/src to reach them, they must go up 3 dirs
        archAddress , connMapAddress, exp_flow_json = self.getUserJsons()

        with open(archAddress, 'rb') as f1, open(connMapAddress, 'rb') as f2:
            for ip in globe.components.devicesIp:
                files = [('arch.json', f1), ('conn.json', f2)]
                address = f'http://{ip}:8484/updateJsonPath'

                try: response = requests.post(address, files=files, timeout=8)
                except requests.exceptions.Timeout:
                    print(f'ERROR: TIMEOUT, COULDN\'T INIT DEVICES!!\nMake sure IPs are correct in {archAddress}')
                    return False

            # response = requests.post(address, data, timeout = 10)
            if globe.jupyterFlag == False:
              print(response.ok, response.status_code)
        time.sleep(1)       # wait for connection to close
        print("Init JSONs sent to devices")

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
   
    def sendDataToSources(self, phase):
        print("\nSending data to sources")
        # <num of sources> Acks for updateCSV():
        globe.pendingAcks += len(globe.components.sources) 
        print(f"waiting for {globe.pendingAcks} acks from {len(globe.components.sources)} sources")
        self.transmitter.updateCSV(phase)

        while globe.pendingAcks > 0:
            time.sleep(0.005)
            pass 

        print("\nData ready in sources")


    def train(self, name = ""):
        # Choose a nem for the current experiment:
        if not name:
            print("\nPlease choose a name for the current experiment:", end = ' ')
            globe.experiment_flow_global.name = input()
        else: 
            globe.experiment_flow_global.name = name
            
        # Create a new folder for the CSVs of the chosen experiment:
        if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{globe.experiment_flow_global.name}'):
            os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{globe.experiment_flow_global.name}')
            os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{globe.experiment_flow_global.name}/Training')
            os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{globe.experiment_flow_global.name}/Prediction')

        globe.experiment_flow_global.emptyExp() # Start a new empty experiment
        self.transmitter.train()
        expResults = self.getQueueData()
        print('Training - Finished\n')
        return expResults

    def contPhase(self, phase):
        self.transmitter.contPhase(phase)
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

    def print_saved_experiments(self):
        if (len(self.experiments) == 0):
            print("No experiments were conducted yet.")
            return

        print("\n---SAVED EXPERIMENTS---\n")
        print("List of saved experiments:")
        for i, exp in enumerate(self.experiments, start=1): 
            print(f"{i}) {exp.name}")

    def plot_loss(self, expNum):
        expForStats = self.experiments[expNum-1]

        # Create a new folder for to save an image of the plot:
        if not os.path.exists(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training'):
            os.mkdir(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training')


        ####### THIS IS TO PICK ONLY A SPECIFIC SOURCE FOR PLOT:

        # numOfCsvs = len(expForStats.trainingResList)
        # print(f"\nThe training phase contains {numOfCsvs} source CSVs:")

        # for i, csvRes in enumerate(expForStats.trainingResList, start=1):
        #     print(f"{i}) {csvRes.name}")

        # while True:
        #     print("\nPlease choose a CSV number for the plot (for multiple CSVs, seperate their numbers with ', '):", end = ' ')       
        #     csvNumsStr = input()

        #     try:
        #         csvNumsList = csvNumsStr.strip().split(',')
        #         csvNumsList = [int(csvNum) for csvNum in csvNumsList]
        #     except ValueError:
        #         print("\nIllegal Input") 
        #         continue
            
        #     if (all(csvNum > 0 and csvNum <= numOfCsvs for csvNum in csvNumsList)): # Check if all CSV indexes are in the correct range.
        #         break
        #     else: print("\nInvalid Input") 

        # Draw the plot using Matplotlib:
        plt.figure(figsize = (30,15), dpi = 150)
        plt.rcParams.update({'font.size': 22})

        for csvRes in expForStats.trainingResList:
            for workerRes in csvRes.workersResList:
                data = workerRes.resList
                plt.plot(data, linewidth = 3)

            expTitle = (expForStats.name)
            plt.title(f"Training - Loss Function - {expTitle}", fontsize=38)
            plt.xlabel('Batch No.', fontsize = 30)
            plt.ylabel('Loss (MSE)', fontsize = 30)
            plt.xlim(left=0)
            plt.ylim(bottom=0)
            plt.legend(csvRes.workers)
            plt.grid(visible=True, which='major', linestyle='-')
            plt.minorticks_on()
            plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)

        plt.show()
        fileName = globe.experiment_flow_global.name
        plt.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training/{fileName}.png')
        print(f'\n{fileName}.png was Saved...')

    def accuracy_matrix(self, expNum):
        expForStats = self.experiments[expNum-1] 

        # Choose the matching (to the original labeled CSV) CSV from the prediction results list:
        numOfCsvs = len(expForStats.predictionResList)

        print(f"\nThe prediction phase contains {numOfCsvs} CSVs:")     ## these are source unlabeled CSVs that need to be compared to test labeled CSVs
        for i, csvRes in enumerate(expForStats.predictionResList, start=1):
            print(f"{i}) {csvRes.name}: samples starting at {csvRes.indexOffset}")

        # while True:
        #     print("\nPlease choose a CSV number for accuracy calculation and confusion matrix (for multiple CSVs, seperate their numbers with ', '):", end = ' ')       
        #     csvNumsStr = input()

        #     try:
        #         csvNumsList = csvNumsStr.split(', ')
        #         csvNumsList = [int(csvNum) for csvNum in csvNumsList]

        #     except ValueError:
        #         print("\nIllegal Input") 
        #         continue       

        #     # Check if all CSV indexes are in the correct range.
        #     if (all(csvNum > 0 and csvNum <= numOfCsvs for csvNum in csvNumsList)): break
        #     else: print("\nInvalid Input") 

        while True:
            print("\nPlease enter the name of the FULL LABELED PREDICTION DATA (including .csv):", end = ' ') 
            labelsCsvPath = input()
            for root, dirnames, filenames in os.walk(globe.INPUT_DATA_PATH):
                for filename in filenames:
                    if filename == labelsCsvPath:
                        labelsCsvPath = os.path.join(root, filename)
                        break

            try:
                labelsCsvDf = pd.read_csv(labelsCsvPath, header=None) #TODO: should expect header= None or 0?
                break

            except OSError: print("\nInvalid path\n")

        # Extract the labels columns from the CSV. Create a list of labels:
        labelsLen = 1
        try: labelsLen = len(expForStats.expFlow["Labels"])
        finally: print(f"assuming {labelsLen} lables")

        labelsSeries = labelsCsvDf.iloc[:,-labelsLen:]  ## this is the full list of only labels

        try: labelNames = expForStats.expFlow["Labels"]      ## get label names from experimnet JSON
        except: labelNames = [str(i) for i in range(labelsLen)] #if no labels, set to 1,2,3....
        
        workersList = []
        workerNeuronRes = {}
        for csvRes in expForStats.predictionResList:
            for worker in csvRes.workers:
                workerNeuronRes[worker] = None      # each worker creates its own [predL, trueL] lists
                workersList.append(worker)

        ## create a different confusion matrix for each label
        predlabels = [[] for i in range(labelsLen)]
        trueLabels = [[] for i in range(labelsLen)]  

        for sourceCSV in expForStats.predictionResList:         #### TODO: match specific batch number of predict vector to test.csv of labels

            # Generate the samples' indexes from the results:
            for worker in sourceCSV.workersResList:
                for batchRes in worker.resList:
                    batchRanges = batchRes.indexRange   # (startSampNum, endSampNum)

                    for ind, sample in enumerate(range(batchRanges[0], batchRanges[1])):
                        for label in range(labelsLen):
                            trueLabels[label].append(str(labelsSeries.iloc[sample,label]))
                            predlabels[label].append(str(round(batchRes.predictions[ind][label])))
                
                workerNeuronRes[worker.name] = (trueLabels, predlabels)

                predlabels = [[] for i in range(labelsLen)]
                trueLabels = [[] for i in range(labelsLen)]
                    
                            # print(f"{worker.name} for sample #{sampleNum} predicted {batch.predictions[i][j]}, real is {labelsSeries.iloc[sampleNum+i,j]}")

        # # Create a confusion matrix based on the results:
        # Another option to solve this problem, is to numerize each classification group (group 1, group 2, ...), 
        # and add legened to show the true label value for each group.
        
                    ################## THIS IS *NOT* FOR MULTICLASS DATA, but for multi-label data
        confMatList = {}
        f, axes = plt.subplots(len(workersList), labelsLen, figsize=(5*labelsLen, 5*len(workersList)))
        axes = axes.ravel()
        for i, worker in enumerate(workersList):
            confMatList[worker] = []

            for j in range(labelsLen):
                confMatList[worker].append(confusion_matrix(workerNeuronRes[worker][0][j], workerNeuronRes[worker][1][j]))

                disp = ConfusionMatrixDisplay(confMatList[worker][j], display_labels=[0, labelNames[j]])
                disp.plot(ax=axes[i*labelsLen+j], values_format='.4g')
                disp.ax_.set_title(f'{worker}, class #{j}\nAccuracy={round(accuracy_score(workerNeuronRes[worker][0][j], workerNeuronRes[worker][1][j]), 3)}')
                if i < len(workersList) - 1:
                    disp.ax_.set_xlabel('') #remove "predicted label"
                if  j != 0:
                    disp.ax_.set_ylabel('') #remove "true label"
                disp.im_.colorbar.remove()  #remove individual colorbars

            # print(classification_report(trueLabels[j], predlabels[j]))

        plt.subplots_adjust(wspace=0.8, hspace=0.15)
        f.colorbar(disp.im_, ax=axes)
        plt.show()

        fileName = sourceCSV.name.rsplit('/', 1)[-1] # If the CSV name contains a path, then take everything to the right of the last '/'.
        disp.figure_.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction/{fileName}.png')
        print(f'\n{fileName}.png Saved...')

        return confMatList
    
    def confusion_stats(self, confList):
        for worker in confList:
            for j, label in enumerate(confList[worker]):
                # Calculate the accuracy and other stats:
                tp, tn, fp, fn = label.ravel()
                acc = (tp + tn) / (tp + tn + fp + fn)
                ppv = tp / (tp + fp)
                tpr = tp / (tp + fn)
                tnr = tn / (tn + fp)
                bacc = (tpr + tnr) / 2
                inf = tpr + tnr - 1

                print(f"{worker}, class #{j}:")
                print("\n")
                print(f"Accuracy acquired (TP+TN / Tot):            {round(acc*100, 3)}%.\n")
                print(f"Balanced Accuracy (TPR+TNR / 2):            {round(bacc*100, 3)}%.\n")
                print(f"Positive Predictive Rate (Precision of P):  {round(ppv*100, 3)}%.\n")
                print(f"True Pos Rate (Sensitivity / Hit Rate):     {round(tpr*100, 3)}%.\n")
                print(f"True Neg Rate (Selectivity):                {round(tnr*100, 3)}%.\n")
                print(f"Informedness (of making decision):          {round(inf*100, 3)}%.\n")
    
    def communication_stats(self):
        self.transmitter.statistics()

    def statistics(self):
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
            self.plot_loss(expNum)

        if (option == 2):
            self.accuracy_matrix(expNum)

        if (option == 3):

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
            self.communication_stats()

                
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
