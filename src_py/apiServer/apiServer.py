################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay, accuracy_score, classification_report, multilabel_confusion_matrix
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
from definitions import *
from logger import *

def is_port_in_use(port: int) -> bool:
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

class ApiServer():
    def __init__(self):       
        self.json_dir_parser = JsonDirParser()
        self.input_data_path = read_nerlconfig(NERLCONFIG_INPUT_DATA_DIR)

        # Create a new folder for the results:
        if not os.path.exists('/usr/local/lib/nerlnet-lib/NErlNet/Results'):
            os.mkdir('/usr/local/lib/nerlnet-lib/NErlNet/Results')

        pass

    def set_json_dir(self, custom_path : str):
        self.json_dir_parser = JsonDirParser(custom_path)

    def help(self):
    #i) data saved as .csv, training file ends with "_Training.csv", prediction with "_Prediction.csv" (may change in future)
        print(
f"""
__________NERLNET CHECKLIST__________
0. Run this Jupyter in the folder of generated .py files!
1. Nerlnet configuration files are located at config directory
   Make sure data and jsons in correct folder, and jsons include the correct paths
    * Data includes: labeled prediction csv, training file, prediction file
    * Prediction CSVs need to be ordered the same!
    * jsonsDir is set to {self.json_dir_parser.get_json_dir_path()}
    * inputDataDir is set to {self.input_data_path}
            
____________API COMMANDS_____________
==========Setting experiment========

-showJsons():                       shows available arch / conn / exp layouts
-printArchParams(Num)               print description of selected arch file
-selectJsons():                     get input from user for arch / conn / exp selection
-setJsons(arch, conn, exp):         set layout in code
-getUserJsons():                    returns the selected arch / conn / exp
-initialization(arch, conn, exp):   set up server for a NerlNet run
-sendJsonsToDevices():              send each NerlNet device the arch / conn jsons to init entities on it
-sendDataToSources(phase(,split)):  phase := "training" | "prediction". split := 1 default (split) | 2 (whole file). send the experiment data to sources (currently happens in beggining of train/predict)

========Running experiment==========
-train():                           start training phase
-predict():                         start prediction phase
-contPhase(phase):                  send another `Batch_size` of a phase (must be called after initial train/predict)

==========Experiment info===========
-print_saved_experiments()          prints saved experiments and their number for statistics later
-plot_loss(ExpNum)                  saves and shows the loss over time of the chosen experiment
-accuracy_matrix(ExpNum, Normalize) Normalize = True | False. shows a graphic for the confusion matrix. Also returns all ConfMat[worker][nueron]
-communication_stats()              prints the communication statistics of the current network. integer => message count, float => avg calc time (ms)

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
        print("Connections:")
        for key, val in connData['connectionsMap'].items():
            print("\t\t", key, ' : ', val)
        globe.experiment_flow_global.printExp()

        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort
        self.experiments = []
        
        print("Initializing the receiver thread...\n")

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        if not is_port_in_use(int(globe.components.receiverPort)):
            self.receiverProblem = threading.Event()
            self.receiverThread = threading.Thread(target = receiver.initReceiver, args = (globe.components.receiverHost, globe.components.receiverPort, self.receiverProblem), daemon = True)
            self.receiverThread.start()   
            # time.sleep(2)
            self.receiverThread.join(2) # After 2 secs, the receiver is either running, or the self.receiverProblem event is set.

            if (self.receiverProblem.is_set()): # If a problem has occured when trying to run the receiver.
                print(f"===================Failed to initialize the receiver using the provided address:==========================\n\
                (http://{globe.components.receiverHost}:{globe.components.receiverPort})\n\
    Please change the 'host' and 'port' values for the 'serverAPI' key in the architecture JSON file.\n")
                sys.exit()

        # Initalize an instance for the transmitter:
        if not hasattr(self, 'transmitter'):
            self.transmitter = Transmitter(self.mainServerAddress, self.input_data_path)

        print("\n***Please remember to execute NerlnetRun.sh on each device before continuing.")
    
    def sendJsonsToDevices(self):
        # Send the content of jsonPath to each devices:
        print("\nSending JSON paths to devices...")

        # Jsons found in NErlNet/inputJsonFiles/{JSON_TYPE}/files.... for entities in src_erl/Comm_layer/http_nerl/src to reach them, they must go up 3 dirs
        archAddress , connMapAddress, exp_flow_json = self.getUserJsons()

        # TODO - Wrong, communication bypasses main server. Need to send json files to main server and main server distributes files
        for ip in globe.components.devicesIp:
            with open(archAddress, 'rb') as arch_json_file, open(connMapAddress, 'rb') as conn_json_file:
                files = [(JSON_FILE_ARCH_REMOTE_NAME, arch_json_file), (JSON_FILE_COMM_REMOTE_NAME, conn_json_file)]
                address = f'http://{ip}:{JSON_INIT_HANDLER_ERL_PORT}/updateJsonPath'

                try: response = requests.post(address, files=files, timeout=8)
                except requests.exceptions.Timeout:
                    print(f'ERROR: TIMEOUT, COULDN\'T INIT DEVICES!!\nMake sure IPs are correct in {archAddress}')
                    return False

            if globe.jupyterFlag == False:
              print(response.ok, response.status_code)
        time.sleep(1)       # wait for connection to close ## TODO: check why
        print("Init JSONs sent to devices")

    def showJsons(self):
        self.json_dir_parser.print_lists()
    
    def printArchParams(self, arch = ""):
        if not arch:
            print("\n Enter arch file number:", end = ' ')
            arch = input()
        selectedArch = self.json_dir_parser.arch_list[int(arch)].get_full_path()
        NetworkComponents(self.json_dir_parser.json_from_path(selectedArch)).printComponents()

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

    def tic(self):
        return time.time()
    
    def toc(self, start):
        return time.time() - start

    def stopServer(self):
        receiver.stop()
        return True

    ## TODO: should be reviewed by Noa, Ohad and David
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
   
        ## TODO: standartize the phase names / make them == .csv file
    def sendDataToSources(self, phase, splitMode = 1):
        print("\nSending data to sources")
        if not globe.CSVsplit:
            globe.CSVsplit = splitMode 

        # 1 ack for mainserver, who waits for all sources
        globe.pendingAcks = 1
        # print(f"waiting for {globe.pendingAcks} acks from {len(globe.components.sources)} sources")
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
            print("No experiments conducted yet.")
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
        workers = []
        for csvRes in expForStats.trainingResList:
            workers.extend(csvRes.workers)
            for workerRes in csvRes.workersResList:
                data = workerRes.resList
                plt.plot(data, linewidth = 3)

        expTitle = (expForStats.name)
        plt.title(f"Training - Loss Function - {expTitle}", fontsize=38)
        plt.xlabel('Batch No.', fontsize = 30)
        plt.ylabel('Loss (MSE)', fontsize = 30)
        plt.yscale('log')
        plt.xlim(left=0)
        plt.ylim(bottom=0)
        plt.legend(workers)
        plt.grid(visible=True, which='major', linestyle='-')
        plt.minorticks_on()
        plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)

        plt.show()
        fileName = globe.experiment_flow_global.name
        plt.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Training/{fileName}.png')
        print(f'\n{fileName}.png was Saved...')

    def accuracy_matrix(self, expNum, normalizeEnabled = False):
        expForStats = self.experiments[expNum-1] 

        # Choose the matching (to the original labeled CSV) CSV from the prediction results list:

        numOfCsvs = len(expForStats.predictionResList)
        print(f"\nThe prediction phase contains {numOfCsvs} CSVs:")     ## these are source unlabeled CSVs that need to be compared to test labeled CSVs
        for i, csvRes in enumerate(expForStats.predictionResList, start=1):
            print(f"{i}) {csvRes.name}: samples starting at {csvRes.indexOffset}")

        # while True:           ####### FOR MULTIPLE CSVs
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

        ################### for single csv:
        # while True:
        # print("\nPlease enter the name of the FULL LABELED PREDICTION DATA (including .csv):", end = ' ') 
        # labelsCsvPath = input()
        labelsCsvPath = f"{expForStats.predictionResList[0].name}_test.csv"
        for root, dirnames, filenames in os.walk(self.input_data_path):
            for filename in filenames:
                if filename == labelsCsvPath:
                    labelsCsvPath = os.path.join(root, filename)
                    try:
                        labelsCsvDf = pd.read_csv(labelsCsvPath, header=None)
                        # break

                    except OSError: print("\nInvalid path\n")
                    break


        # Extract the labels columns from the CSV. Create a list of labels:
        labelsLen = 1
        try: labelsLen = len(expForStats.expFlow["Labels"])
        finally: print(f"assuming {labelsLen} lables")

        try: labelNames = expForStats.expFlow["Labels"]      ## get label names from experimnet JSON
        except: labelNames = [str(i) for i in range(labelsLen)] #if no labels, set to 1,2,3....
        
        labelsSeries = labelsCsvDf.iloc[:,-labelsLen:]  ## this is the full list of only labels

        workersList = []
        workerNeuronRes = {}
        for csvRes in expForStats.predictionResList:
            for worker in csvRes.workers:
                workerNeuronRes[worker] = None      # each worker creates its own [predL, trueL] lists
                workersList.append(worker)

        ## create a different confusion matrix for each label

        for sourceCSV in expForStats.predictionResList:         #### TODO: match specific batch number of predict vector to test.csv of labels

            # Generate the samples' indexes from the results:
            for worker in sourceCSV.workersResList:
                predlabels = [[] for i in range(labelsLen)]
                trueLabels = [[] for i in range(labelsLen)]  
                
                for batchRes in worker.resList:
                    batchRanges = batchRes.indexRange   # (startSampNum, endSampNum)
                    # print(f"testing sample ranges: {batchRanges}")
                    for ind, sample in enumerate(range(batchRanges[0], batchRanges[1])):
                        for label in range(labelsLen):
                            truelabel = str(labelsSeries.iloc[sample,label])
                            assert truelabel == '0' or truelabel == '1', f"true label at {sample},{label} is {truelabel}"
                            if batchRes.predictions[ind][label] > 0.5 :
                                predlabel = '1'
                            else:
                                predlabel = '0'
                            # predlabel = str(round(batchRes.predictions[ind][label]))
                            trueLabels[label].append(truelabel)
                            predlabels[label].append(predlabel)
                # print(f"for worker {worker.name} have true={trueLabels}, pred={predlabels}")
                workerNeuronRes[worker.name] = (trueLabels, predlabels)
                    
                            # print(f"{worker.name} for sample #{sampleNum} predicted {batch.predictions[i][j]}, real is {labelsSeries.iloc[sampleNum+i,j]}")

        # # Create a confusion matrix based on the results:
        
                    ################## THIS IS *NOT* FOR MULTICLASS DATA, but for multi-label data (output neurons are binary)
        confMatList = {}
        f, axes = plt.subplots(len(workersList), labelsLen, figsize=(globe.MATRIX_DISP_SCALING*labelsLen, globe.MATRIX_DISP_SCALING*len(workersList)))
        for i, worker in enumerate(workersList):
            confMatList[worker] = [[] for i in range(labelsLen)]

            for j in range(labelsLen):
                # print(f"worker {worker}, has {len(workerNeuronRes[worker][TRUE_LABLE_IND])} labels, with {len(workerNeuronRes[worker][TRUE_LABLE_IND][j])} samples")
                # print(f"confusion {worker}:{j}, has is of {workerNeuronRes[worker][TRUE_LABLE_IND][j]}, {workerNeuronRes[worker][PRED_LABLE_IND][j]}")
                if normalizeEnabled == True :
                    confMatList[worker][j] = confusion_matrix(workerNeuronRes[worker][globe.TRUE_LABLE_IND][j], workerNeuronRes[worker][globe.PRED_LABLE_IND][j], normalize='all')
                else:
                    confMatList[worker][j] = confusion_matrix(workerNeuronRes[worker][globe.TRUE_LABLE_IND][j], workerNeuronRes[worker][globe.PRED_LABLE_IND][j])
                # print(confMatList[worker][j])
                disp = ConfusionMatrixDisplay(confMatList[worker][j], display_labels=["X", labelNames[j]])
                disp.plot(ax=axes[i, j], colorbar=False)
                disp.ax_.set_title(f'{worker}, class #{j}\nAccuracy={round(accuracy_score(workerNeuronRes[worker][globe.TRUE_LABLE_IND][j], workerNeuronRes[worker][globe.PRED_LABLE_IND][j]), 3)}')
                if i < len(workersList) - 1:
                    disp.ax_.set_xlabel('') #remove "predicted label"
                if  j != 0:
                    disp.ax_.set_ylabel('') #remove "true label"
                # disp.im_.colorbar.remove()  #remove individual colorbars

        plt.subplots_adjust(wspace=1, hspace=0.15)        ## adjust for spacing between matrix
        f.colorbar(disp.im_, ax=axes)
        plt.show()

        fileName = sourceCSV.name.rsplit('/', 1)[-1] # If the CSV name contains a path, then take everything to the right of the last '/'.
        disp.figure_.savefig(f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction/{fileName}.png')
        print(f'\n{fileName}.png Saved...')
        
        ## print and save prediction stats
        statFileName = f'/usr/local/lib/nerlnet-lib/NErlNet/Results/{expForStats.name}/Prediction/stats.txt'
        if os.path.exists(statFileName): os.remove(statFileName)
        statFile = open(statFileName, "a")

        for worker in confMatList:
            for j, label in enumerate(confMatList[worker]):
                # Calculate the accuracy and other stats:
                tn, fp, fn, tp = label.ravel()
                acc = (tp + tn) / (tp + tn + fp + fn)
                ppv = tp / (tp + fp)
                tpr = tp / (tp + fn)
                tnr = tn / (tn + fp)
                bacc = (tpr + tnr) / 2
                inf = tpr + tnr - 1
                
                print(f"{worker}, class #{j}:")
                print(f"Accuracy acquired (TP+TN / Tot):            {round(acc*100, 3)}%.")
                print(f"Balanced Accuracy (TPR+TNR / 2):            {round(bacc*100, 3)}%.")
                print(f"Positive Predictive Rate (Precision of P):  {round(ppv*100, 3)}%.")
                print(f"True Pos Rate (Sensitivity / Hit Rate):     {round(tpr*100, 3)}%.")
                print(f"True Neg Rate (Selectivity):                {round(tnr*100, 3)}%.")
                print(f"Informedness (of making decision):          {round(inf*100, 3)}%.\n\n")

                statFile.write(f"{worker}, class #{j}:\n")
                statFile.write(f"Accuracy acquired (TP+TN / Tot):            {round(acc*100, 3)}%.\n")
                statFile.write(f"Balanced Accuracy (TPR+TNR / 2):            {round(bacc*100, 3)}%.\n")
                statFile.write(f"Positive Predictive Rate (Precision of P):  {round(ppv*100, 3)}%.\n")
                statFile.write(f"True Pos Rate (Sensitivity / Hit Rate):     {round(tpr*100, 3)}%.\n")
                statFile.write(f"True Neg Rate (Selectivity):                {round(tnr*100, 3)}%.\n")
                statFile.write(f"Informedness (of making decision):          {round(inf*100, 3)}%.\n")
            print("=========================================================\n")
            statFile.write("=========================================================\n")
        statFile.close()
        print(f'\nstats file saved...')
    
    def communication_stats(self):
        self.transmitter.statistics()

    def export_results(self, expNum):
        expForStats = self.experiments[expNum-1]

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
            self.export_results(expNum)

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
