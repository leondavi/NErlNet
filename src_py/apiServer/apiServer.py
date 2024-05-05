################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import time
import threading
import sys
from experiment_flow import *
from pathlib import Path
from jsonDirParser import JsonDirParser
from transmitter import Transmitter
from networkComponents import NetworkComponents
import globalVars as globe
import receiver
from definitions import *
from logger import *
from NerlComDB import *
from events_sync import *

class ApiServer():
    def __init__(self):       
        self.json_dir_parser = JsonDirParser()
        self.experiments_dict = {}
        self.current_exp = None
        self.apiserver_event_sync = EventSync() # pay attention! there are two kinds of syncs one for experiment phase events and one for api-server events

        # Create a new folder for the results:
        Path(EXPERIMENT_RESULTS_PATH).mkdir(parents=True, exist_ok=True)

    def get_experiment_flow(self, exp_name : str) -> ExperimentFlow:
        return self.experiments_dict[exp_name] if exp_name in self.experiments_dict else None

    def set_json_dir(self, custom_path : str):
        self.json_dir_parser = JsonDirParser(custom_path)

    def reset(self):
        return self.__init__()

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
TRAINING_STR = "Training"
PREDICTION_STR = "Prediction"
        """)
    
    def __new_experiment(self, experiment_name : str, json_path: str, batch_size: int, network_componenets: NetworkComponents):
        assert experiment_name not in self.experiments_dict, "experiment name exists!"
        self.experiments_dict[experiment_name] = ExperimentFlow(experiment_name, batch_size, network_componenets)
        self.experiments_dict[experiment_name].parse_experiment_flow_json(json_path)

    def experiment_focused_on(self, experiment_name):
        assert experiment_name in self.experiments_dict, "cannot focus on experiment that has never been created!"
        globe.experiment_focused_on = self.get_experiment_flow(experiment_name) # Get experiment instance from expirments dict
        self.current_exp = globe.experiment_focused_on # TODO the objective is to get rid of this global definitions

    def initialization(self, experiment_name : str, dc_json: str, conn_map_json, experiment_flow_json):
        dcData = self.json_dir_parser.json_from_path(dc_json)
        connData = self.json_dir_parser.json_from_path(conn_map_json)
        batch_size = int(dcData["nerlnetSettings"]["batchSize"])

        globe.components = NetworkComponents(dcData) # move network component into experiment class
        # comDB = NerlComDB(globe.components)
        self.__new_experiment(experiment_name, experiment_flow_json, batch_size, globe.components) # create new experiment
        self.experiment_focused_on(experiment_name)

        globe.components.printComponents()
        LOG_INFO("Connections:")
        for key, val in connData['connectionsMap'].items():
            LOG_INFO(f"\t\t {key} : {val}")
        globe.experiment_focused_on.print()

        mainServerIP = globe.components.mainServerIp
        mainServerPort = globe.components.mainServerPort
        self.mainServerAddress = 'http://' + mainServerIP + ':' + mainServerPort

        # Initalize an instance for the transmitter:
        if not hasattr(self, 'transmitter'):
            self.transmitter = Transmitter(self.current_exp, self.mainServerAddress)
        
        LOG_INFO("Initializing ApiServer receiver thread")

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        if is_port_free(int(globe.components.receiverPort)):
            self.receiverProblem = threading.Event()
            self.receiverThread = threading.Thread(target = receiver.initReceiver, args = (globe.components, self.transmitter, self.receiverProblem, self.apiserver_event_sync), daemon = True)
            self.receiverThread.start()   
            # time.sleep(2)
            self.receiverThread.join(2) # After 2 secs, the receiver is either running, or the self.receiverProblem event is set.

            if (self.receiverProblem.is_set()): # If a problem has occured when trying to run the receiver.
                LOG_ERROR(f"===================Failed to initialize the receiver using the provided address:==========================\n\
                (http://{globe.components.receiverIp}:{globe.components.receiverPort})\n\
                Please change the 'host' and 'port' values for the 'serverAPI' key in the architecture JSON file.\n")
                sys.exit()


        LOG_INFO("*** Remember to execute NerlnetRun.sh on each device before running the experiment! ***")
        
                
    def send_jsons_to_devices(self): #User Api
        archAddress , connMapAddress, _ = self.getUserJsons()
        with open(archAddress, 'rb') as dc_json_file, open(connMapAddress, 'rb') as conn_json_file:
            files = [(DC_FILE_ARCH_REMOTE_NAME, dc_json_file), (JSON_FILE_COMM_REMOTE_NAME, conn_json_file)] # files names should be identical to the ones defined as ?LOCAL_DC_FILE_NAME ?LOCAL_COMM_FILE_NAME
            self.apiserver_event_sync.set_event_wait(EventSync.SEND_JSONS)
            self.transmitter.send_jsons_to_devices(files)
            self.apiserver_event_sync.sync_on_event(EventSync.SEND_JSONS)
            LOG_INFO("Sending distributed configurations to devices is completed")


    def showJsons(self):
        self.json_dir_parser.print_lists()
    
    def printArchParams(self, arch = ""):
        if not arch:
            print("\n Enter arch file number:", end = ' ')
            arch = input()
        selectedArch = self.json_dir_parser.dc_list[int(arch)].get_full_path()
        NetworkComponents(self.json_dir_parser.json_from_path(selectedArch)).printComponents()

    def setJsons(self, dc_num : int, conn_num : int, exp_num : int):
        self.json_dir_parser.set_dc_connmap_experiment(dc_num, conn_num, exp_num)
    
    def getUserJsons(self):
        return self.json_dir_parser.get_user_selection_files()
        
    def getTransmitter(self):
        return self.transmitter

    def tic(self):
        return time.time()
    
    def toc(self, start):
        return time.time() - start

    def terminate(self):
        self.apiserver_event_sync.set_event_wait(EventSync.TERMINATE)
        self.transmitter.terminate()
        self.apiserver_event_sync.sync_on_event(EventSync.TERMINATE)
        return True
    
    def send_data_to_sources(self, csv_dataset: CsvDataSet, experiment_phase: ExperimentPhase, events_sync_inst: EventSync):
        LOG_INFO("Sending data to sources")
        sources_pieces_list = experiment_phase.get_sources_pieces()
        source_files_to_send = []  # list of csv's paths to send to sources
        for source_piece_inst in sources_pieces_list:
            source_generated_csv_path = csv_dataset.generate_source_piece_ds_csv_file(source_piece_inst, experiment_phase.get_phase_type())
            source_files_to_send.append(source_generated_csv_path)

        events_sync_inst.set_event_wait(EventSync.UPDATE_CSV)
        self.transmitter.update_csv(source_files_to_send, sources_pieces_list)
        events_sync_inst.sync_on_event(EventSync.UPDATE_CSV)
        LOG_INFO("Data is ready in sources")

    def run_current_experiment_phase(self):
        current_exp_phase = self.current_exp.get_current_experiment_phase()
        LOG_INFO(f"Experiment phase: {current_exp_phase.get_name()} of type {current_exp_phase.get_phase_type()} starts running...")
        csv_dataset_inst = self.current_exp.get_csv_dataset()
        events_sync_inst = self.current_exp.get_events_sync()
        
        send_jsons_event = self.apiserver_event_sync.get_event_status(EventSync.SEND_JSONS)
        assert send_jsons_event == EventSync.DONE, "Jsons not sent to devices yet"

        self.send_data_to_sources(csv_dataset_inst, current_exp_phase, events_sync_inst)

        events_sync_inst.set_event_wait(EventSync.UPDATE_PHASE)
        self.transmitter.clients_set_phase(current_exp_phase.get_phase_type())
        events_sync_inst.sync_on_event(EventSync.UPDATE_PHASE)

        events_sync_inst.set_event_wait(EventSync.START_CASTING)
        self.transmitter.start_casting(current_exp_phase) # Source start sending data to workers
        events_sync_inst.sync_on_event(EventSync.START_CASTING)

        LOG_INFO(f"Processing experiment phase data")
        current_exp_phase.process_experiment_phase_data()
        LOG_INFO(f"Processing experiment phase data completed")

        LOG_INFO(f"Start generating communication statistics for {current_exp_phase.get_name()} of type {current_exp_phase.get_phase_type()}")
        self.communication_stats()

        LOG_INFO(f"Phase of {current_exp_phase.get_name()} {current_exp_phase.get_phase_type()} completed")



    def next_experiment_phase(self):
        current_exp_flow = globe.experiment_focused_on
        events_sync_inst = current_exp_flow.get_events_sync()
        events_sync_inst.reset() # preparing for next phase 
        current_exp_flow.current_exp_phase_index += 1
        if not self.experiment_phase_is_valid():
            LOG_WARNING("No more phases to run")
            return False
        return True

    def communication_stats(self):
        assert self.experiment_phase_is_valid(), "No valid experiment phase"
        events_sync_inst = self.current_exp.get_events_sync()
        self.transmitter.statistics(events_sync_inst)

    def experiment_phase_is_valid(self):
        current_exp_flow = globe.experiment_focused_on
        return current_exp_flow.current_exp_phase_index < len(current_exp_flow.exp_phase_list)
