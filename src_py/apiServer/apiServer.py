################################################
# Nerlnet - 2024 GPL-3.0 license
# Authors: Noa Shapira, Ohad Adi, David Leon
#          Guy Perets, Haran Cohen, Dor Yerchi
################################################

import time
import threading
import sys
import os
import json
import tempfile
from contextlib import ExitStack
from singleton import Singleton
from huggingface_hub import HfApi, utils , snapshot_download
from experiment_flow import *
from pathlib import Path
from jsonDirParser import JsonDirParser
from transmitter import Transmitter
from networkComponents import NetworkComponents
from nerlPlanner.JsonDistributedConfigDefs import *
import globalVars as globe
import receiver
from definitions import *
from logger import *
from NerlComDB import *
from events_sync import *
from apiServerHelp import API_SERVER_HELP_STR

class ApiServer(metaclass=Singleton):
    def __init__(self):
        self.json_dir_parser = JsonDirParser()
        self.experiments_dict = {}
        self.current_exp = None
        self.apiserver_event_sync = EventSync() # pay attention! there are two kinds of syncs one for experiment phase events and one for api-server events
        self.next_expertiment_phase_exist = True      # flag to check if there are more phases to run

        # Create a new folder for the results:
        Path(EXPERIMENT_RESULTS_PATH).mkdir(parents=True, exist_ok=True)

    def get_experiment_flow(self, exp_name : str) -> ExperimentFlow:
        return self.experiments_dict[exp_name] if exp_name in self.experiments_dict else None

    def set_json_dir(self, custom_path : str):
        self.json_dir_parser = JsonDirParser(custom_path)

    def reset(self):
        return self.__init__()

    def help(self):
        print(API_SERVER_HELP_STR)        
    
    def __new_experiment(self, experiment_name : str, json_path: str, batch_size: int, network_componenets: NetworkComponents, csv_path = ""):
        assert experiment_name not in self.experiments_dict, "experiment name exists!"
        self.experiments_dict[experiment_name] = ExperimentFlow(experiment_name, batch_size, network_componenets)
        self.experiments_dict[experiment_name].parse_experiment_flow_json(json_path, csv_path)

    def experiment_focused_on(self, experiment_name):
        assert experiment_name in self.experiments_dict, "cannot focus on experiment that has never been created!"
        globe.experiment_focused_on = self.get_experiment_flow(experiment_name) # Get experiment instance from expirments dict
        self.current_exp = globe.experiment_focused_on # TODO the objective is to get rid of this global definitions

    def initialization(self, experiment_name : str, dc_json: str, conn_map_json, experiment_flow_json, csv_path = ""):
        dcData = self.json_dir_parser.json_from_path(dc_json)
        connData = self.json_dir_parser.json_from_path(conn_map_json)
        batch_size = int(dcData["nerlnetSettings"]["batchSize"])

        globe.components = NetworkComponents(dcData) # move network component into experiment class
        # comDB = NerlComDB(globe.components)
        self.__new_experiment(experiment_name, experiment_flow_json, batch_size, globe.components, csv_path) # create new experiment
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
        torch_assets = self._get_torch_assets()

        with ExitStack() as stack:
            dc_stream = self._prepare_dc_stream(stack, archAddress, torch_assets)
            conn_stream = stack.enter_context(open(connMapAddress, 'rb'))
            files = [
                (DC_FILE_ARCH_REMOTE_NAME, (os.path.basename(archAddress), dc_stream, 'application/json')),
                (JSON_FILE_COMM_REMOTE_NAME, (os.path.basename(connMapAddress), conn_stream, 'application/json'))
            ]
            torch_payloads = self._build_torch_file_payloads(stack, torch_assets)
            if torch_payloads:
                LOG_INFO(f"Attaching {len(torch_payloads)} Torch model artifact(s): {list(torch_assets.keys())}")
                files.extend(torch_payloads)

            self.apiserver_event_sync.set_event_wait(EventSync.SEND_JSONS)
            self.transmitter.send_jsons_to_devices(files)
            self.apiserver_event_sync.sync_on_event(EventSync.SEND_JSONS)
            LOG_INFO("Sending distributed configurations to devices is completed")

    def _get_torch_assets(self):
        components = getattr(globe, 'components', None)
        if not components or not hasattr(components, 'get_torch_model_assets'):
            return {}
        return components.get_torch_model_assets()

    def _prepare_dc_stream(self, stack: ExitStack, dc_path: str, torch_assets: dict):
        if not torch_assets:
            return stack.enter_context(open(dc_path, 'rb'))

        with open(dc_path, 'r', encoding='utf-8') as dc_file_obj:
            dc_dict = json.load(dc_file_obj)

        mutated = False
        model_sha_section = dc_dict.get(KEY_MODEL_SHA, {})
        for model_sha, asset in torch_assets.items():
            if model_sha in model_sha_section:
                if model_sha_section[model_sha].get('pt_path') != asset['remote_path']:
                    model_sha_section[model_sha]['pt_path'] = asset['remote_path']
                    mutated = True

        if not mutated:
            return stack.enter_context(open(dc_path, 'rb'))

        temp_file = tempfile.NamedTemporaryFile(mode='w+b', suffix='_torch_dc.json', delete=False)
        stack.callback(lambda path=temp_file.name: os.remove(path))
        temp_file.write(json.dumps(dc_dict, indent=4).encode('utf-8'))
        temp_file.flush()
        temp_file.seek(0)
        return stack.enter_context(temp_file)

    def _build_torch_file_payloads(self, stack: ExitStack, torch_assets: dict):
        payloads = []
        for _, asset in torch_assets.items():
            artifact_handle = stack.enter_context(open(asset['local_path'], 'rb'))
            payloads.append(
                (asset['remote_path'], (os.path.basename(asset['local_path']), artifact_handle, 'application/octet-stream'))
            )
        return payloads


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
            source_generated_csv_path = source_piece_inst.get_pointer_to_sourcePiece_CsvDataSet()
            source_files_to_send.append(source_generated_csv_path)
        LOG_INFO("Done sending data to source")

        events_sync_inst.set_event_wait(EventSync.UPDATE_CSV)
        self.transmitter.update_csv(source_files_to_send, sources_pieces_list)
        events_sync_inst.sync_on_event(EventSync.UPDATE_CSV)
        LOG_INFO("Data is ready in sources")

    def run_current_experiment_phase(self):
        if not self.next_expertiment_phase_exist:         # don't allow calling the same phase twice 
            LOG_WARNING("experiment override is not supported!")
        else:
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
            
            self.next_expertiment_phase_exist = False  


    def next_experiment_phase(self):
        """
        Returns - None if noe more experiments
                  next phase type (training or prediction)
        """
        current_exp_flow = globe.experiment_focused_on
        events_sync_inst = current_exp_flow.get_events_sync()
        events_sync_inst.reset() # preparing for next phase 
        current_exp_flow.current_exp_phase_index += 1
        if not self.experiment_phase_is_valid():
            LOG_WARNING("No more phases to run")
            self.next_expertiment_phase_exist = False
            return None
        else:
            self.next_expertiment_phase_exist = True
            next_phase_type = self.current_exp.get_current_experiment_phase().get_phase_type()
            return next_phase_type

    def communication_stats(self):
        assert self.experiment_phase_is_valid(), "No valid experiment phase"
        events_sync_inst = self.current_exp.get_events_sync()
        self.transmitter.statistics(events_sync_inst)

    def experiment_phase_is_valid(self):
        current_exp_flow = globe.experiment_focused_on
        return current_exp_flow.current_exp_phase_index < len(current_exp_flow.exp_phase_list)
    
    def run_all_experiment_phases(self):
        """
        Runs all experiment phases sequentially from the experiment JSON file.
        
        This function iterates through all phases defined in the experiment flow JSON,
        executing each phase in order (training phases followed by prediction phases).
        It collects statistics for each completed phase and returns them as a list.
        
        The function will:
        1. Print information about the current phase being executed (name and type)
        2. Execute each phase using run_current_experiment_phase()
        3. Generate and collect Stats objects for each completed phase
        4. Move to the next phase using next_experiment_phase()
        5. Continue until all phases are completed
        
        Returns:
            list: A list of Stats objects, one for each successfully completed phase.
                  Each Stats object contains performance metrics, communication statistics,
                  and other phase-specific data.
        
        Raises:
            AssertionError: If no valid experiment is currently focused or if required
                           setup (initialization, send_jsons_to_devices) is not completed.
        
        Example:
            # After initialization and sending JSONs to devices
            api_server.send_jsons_to_devices()
            all_stats = api_server.run_all_experiment_phases()
            
            # Process results
            for i, stats in enumerate(all_stats):
                print(f"Phase {i+1}: {stats.get_name()} ({stats.get_phase()})")
                if stats.get_phase() == "training":
                    loss_data = stats.get_loss_ts()
                elif stats.get_phase() == "prediction":
                    confusion_matrices = stats.get_confusion_matrices()
        
        Note:
            - Requires that initialization() and send_jsons_to_devices() have been called first
            - All NerlNet devices must be running and accessible
            - The experiment JSON must contain valid phase definitions
        """
        # Ensure we have a valid experiment focused
        if self.current_exp is None:
            raise AssertionError("No experiment is currently focused. Call initialization() and experiment_focused_on() first.")
        
        # Ensure JSONs have been sent to devices
        send_jsons_event = self.apiserver_event_sync.get_event_status(EventSync.SEND_JSONS)
        if send_jsons_event != EventSync.DONE:
            raise AssertionError("JSONs must be sent to devices first. Call send_jsons_to_devices() before running phases.")
        
        all_phases_stats = []
        
        # Get the initial phase information
        current_exp_flow = self.current_exp
        total_phases = len(current_exp_flow.exp_phase_list)
        
        if total_phases == 0:
            LOG_WARNING("No experiment phases found in the experiment flow")
            return all_phases_stats
        
        LOG_INFO(f"Starting to run all {total_phases} experiment phases for experiment: {current_exp_flow.get_exp_name()}")
        
        # Run phases sequentially
        phase_count = 1
        while True:
            if not self.next_expertiment_phase_exist:
                LOG_WARNING("No valid experiment phase available to run")
                break
                
            current_phase = current_exp_flow.get_current_experiment_phase()
            phase_name = current_phase.get_name()
            phase_type = current_phase.get_phase_type()
            
            LOG_INFO(f"Running phase {phase_count}/{total_phases}: '{phase_name}' (Type: {phase_type})")
            
            try:
                # Run the current phase
                self.run_current_experiment_phase()
                
                # Generate stats for the completed phase
                phase_stats = current_exp_flow.generate_stats(current_phase)
                all_phases_stats.append(phase_stats)
                
                LOG_INFO(f"Completed phase {phase_count}/{total_phases}: '{phase_name}' ({phase_type})")
                
            except Exception as e:
                LOG_ERROR(f"Error running phase {phase_count}/{total_phases} '{phase_name}': {str(e)}")
                # Continue with next phase instead of stopping completely
                
            # Move to next phase
            next_phase_type = self.next_experiment_phase()
            if next_phase_type is None:
                LOG_INFO("All experiment phases completed successfully")
                break
                
            phase_count += 1
            
        LOG_INFO(f"Finished running all experiment phases. Total phases executed: {len(all_phases_stats)}")
        return all_phases_stats
    
    def list_datasets(self):
        with open(HF_DATA_REPO_PATHS_JSON) as file:
            repo_ids = json.load(file)
        api = HfApi()
        datasets = {}
        try:
            for repo in repo_ids["datasets"]:
                files = api.list_repo_files(repo_id=repo["id"], repo_type="dataset")
                repo_csv_files = [file for file in files if file.endswith('.csv')]
                datasets[repo["id"]] = repo_csv_files
            for i , (repo_name , files) in enumerate(datasets.items()):
                print(f'{i}. {repo_name}: {files}')
        except utils._errors.RepositoryNotFoundError:
            LOG_INFO(f"Failed to find the repository '{repo}'. Check your '{HF_DATA_REPO_PATHS_JSON}' file or network access.")
            
    def download_dataset(self, repo_idx : int, download_dir_path : str = DEFAULT_NERLNET_TMP_DATA_DIR):
        with open(HF_DATA_REPO_PATHS_JSON) as file:
            repo_ids = json.load(file)
        try:
            if isinstance(repo_idx, int):
                repo_idx = [repo_idx]
            for repo in repo_ids["datasets"]:
                if repo["idx"] in repo_idx:
                    repo_id = repo["id"]
                    full_path_to_repo = f'{download_dir_path}/{repo["name"]}'
                    if not os.path.exists(full_path_to_repo):
                        os.makedirs(full_path_to_repo)
                    snapshot_download(repo_id=repo_id, local_dir=f'{full_path_to_repo}', repo_type="dataset")
                    LOG_INFO(f"Files downloaded to {download_dir_path}/{repo['name']}")
        except utils._errors.RepositoryNotFoundError:
            LOG_INFO(f"Failed to find the repository '{repo}'. Check your '{HF_DATA_REPO_PATHS_JSON}' file or network access.")
        
    
    def add_repo_to_datasets_list(self, repo_id , name : str = "" , description : str = ""):
        try:
            api = HfApi()
            api.list_repo_files(repo_id=repo_id , repo_type="dataset")
        except utils._errors.RepositoryNotFoundError:
            print("Failed to find the repository. Check your 'repo_id' and network access.")
            return
        with open(HF_DATA_REPO_PATHS_JSON) as file:
            repo_ids = json.load(file)
        if repo_id not in [repo["id"] for repo in repo_ids["datasets"]]:
            repo_ids["datasets"].append({"id": repo_id , "idx": len(repo_ids["datasets"]) , "name": name , "description": description})
        else:
            print(f"Repository {repo_id} already exists in the hf_repo_ids.json")
            return
        with open(HF_DATA_REPO_PATHS_JSON, 'w') as file:
            json.dump(repo_ids, file, indent=4)
            print(f"Repository {repo_id} added to the hf_repo_ids.json")