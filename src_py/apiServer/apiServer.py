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
import socket
from contextlib import ExitStack
from singleton import Singleton
from huggingface_hub import HfApi, snapshot_download

try:
    from huggingface_hub.errors import RepositoryNotFoundError
except Exception:
    try:
        from huggingface_hub.utils import RepositoryNotFoundError
    except Exception:
        try:
            from huggingface_hub.utils._errors import RepositoryNotFoundError
        except Exception:
            class RepositoryNotFoundError(Exception):
                pass
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
        self.explicit_json_paths = None
        self.apiserver_event_sync = EventSync() # pay attention! there are two kinds of syncs one for experiment phase events and one for api-server events
        self.next_expertiment_phase_exist = True      # flag to check if there are more phases to run
        self.receiverThread = None
        self.receiverProblem = None
        self._receiver_bind = None  # (ip, port) currently used by this ApiServer receiver thread
        self._runtime_receiver_port_override = None

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

    @staticmethod
    def _get_api_server_ip_from_dc(dc_data: dict) -> str:
        api_device_ip = ""
        for device in dc_data.get("devices", []):
            entities_raw = str(device.get("entities", ""))
            entities = [entity.strip() for entity in entities_raw.split(",") if entity.strip()]
            if "apiServer" in entities:
                api_device_ip = str(device.get("ipv4", "")).strip()
                break
        if not api_device_ip:
            raise RuntimeError("Unable to resolve apiServer IP from distributed config devices.")
        return api_device_ip

    @staticmethod
    def _get_configured_api_server_port(dc_data: dict) -> int:
        api_server_cfg = dc_data.get("apiServer", {})
        raw_port = api_server_cfg.get("port", 0)
        try:
            port = int(raw_port)
        except (TypeError, ValueError):
            raise RuntimeError(f"Invalid apiServer.port value in distributed config: {raw_port!r}")
        if port <= 0:
            raise RuntimeError(f"Invalid apiServer.port value in distributed config: {raw_port!r}")
        return port

    @staticmethod
    def _set_api_server_port_in_dc(dc_data: dict, new_port: int):
        api_server_cfg = dc_data.setdefault("apiServer", {})
        original_value = api_server_cfg.get("port")
        if isinstance(original_value, str):
            api_server_cfg["port"] = str(new_port)
        else:
            api_server_cfg["port"] = int(new_port)

    @staticmethod
    def _parse_receiver_port_candidates_from_env() -> list:
        default_range = "18082-18182"
        raw = str(os.getenv("NERLNET_API_RECEIVER_PORT_RANGE", default_range)).strip()
        tokens = [token.strip() for token in raw.split(",") if token.strip()]
        candidates = []
        seen = set()
        for token in tokens:
            if "-" in token:
                start_raw, end_raw = token.split("-", 1)
                try:
                    start = int(start_raw.strip())
                    end = int(end_raw.strip())
                except ValueError:
                    continue
                if end < start:
                    start, end = end, start
                for port in range(start, end + 1):
                    if 0 < port <= 65535 and port not in seen:
                        candidates.append(port)
                        seen.add(port)
            else:
                try:
                    port = int(token)
                except ValueError:
                    continue
                if 0 < port <= 65535 and port not in seen:
                    candidates.append(port)
                    seen.add(port)
        return candidates

    @staticmethod
    def _reserve_ephemeral_port(host: str) -> int:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.bind((host, 0))
            return int(sock.getsockname()[1])

    def _find_fallback_receiver_port(self, receiver_ip: str, configured_port: int):
        for candidate in self._parse_receiver_port_candidates_from_env():
            if candidate == configured_port:
                continue
            if is_port_free(candidate, receiver_ip):
                return candidate
        try:
            ephemeral_candidate = self._reserve_ephemeral_port(receiver_ip)
            if ephemeral_candidate != configured_port and is_port_free(ephemeral_candidate, receiver_ip):
                return ephemeral_candidate
        except OSError:
            return None
        return None

    def _resolve_receiver_port(self, receiver_ip: str, configured_port: int) -> int:
        receiver_thread_alive = self.receiverThread is not None and self.receiverThread.is_alive()
        active_bind = self._receiver_bind if isinstance(self._receiver_bind, tuple) else None

        if is_port_free(configured_port, receiver_ip):
            return configured_port

        if receiver_thread_alive and active_bind == (receiver_ip, configured_port):
            LOG_INFO(
                f"ApiServer receiver is already running on "
                f"http://{receiver_ip}:{configured_port}; reusing existing thread"
            )
            return configured_port

        if receiver_thread_alive and active_bind and active_bind[0] == receiver_ip:
            active_port = int(active_bind[1])
            if not is_port_free(active_port, receiver_ip):
                LOG_WARNING(
                    f"Configured ApiServer receiver port is busy "
                    f"(http://{receiver_ip}:{configured_port}); "
                    f"reusing active receiver thread on http://{receiver_ip}:{active_port}"
                )
                return active_port

        fallback_port = self._find_fallback_receiver_port(receiver_ip, configured_port)
        if fallback_port is not None:
            LOG_WARNING(
                f"Configured ApiServer receiver port is busy "
                f"(http://{receiver_ip}:{configured_port}); "
                f"switching to fallback port {fallback_port}"
            )
            return fallback_port

        raise RuntimeError(
            f"ApiServer receiver port is busy at http://{receiver_ip}:{configured_port} "
            f"and no fallback port could be allocated."
        )
    
    def __new_experiment(self, experiment_name : str, json_path: str, batch_size: int, network_componenets: NetworkComponents, csv_path = ""):
        assert experiment_name not in self.experiments_dict, "experiment name exists!"
        self.experiments_dict[experiment_name] = ExperimentFlow(experiment_name, batch_size, network_componenets)
        self.experiments_dict[experiment_name].parse_experiment_flow_json(json_path, csv_path)

    def experiment_focused_on(self, experiment_name):
        assert experiment_name in self.experiments_dict, "cannot focus on experiment that has never been created!"
        globe.experiment_focused_on = self.get_experiment_flow(experiment_name) # Get experiment instance from expirments dict
        self.current_exp = globe.experiment_focused_on # TODO the objective is to get rid of this global definitions

    def initialization(self, experiment_name : str, dc_json: str, conn_map_json, experiment_flow_json, csv_path = ""):
        # Each experiment initialization should start with a fresh ApiServer
        # event-sync state to avoid stale DONE/ERROR states from prior runs.
        self.apiserver_event_sync.reset()

        dcData = self.json_dir_parser.json_from_path(dc_json)
        configured_receiver_ip = self._get_api_server_ip_from_dc(dcData)
        configured_receiver_port = self._get_configured_api_server_port(dcData)
        effective_receiver_port = self._resolve_receiver_port(
            configured_receiver_ip,
            configured_receiver_port
        )
        if effective_receiver_port != configured_receiver_port:
            self._set_api_server_port_in_dc(dcData, effective_receiver_port)
            self._runtime_receiver_port_override = int(effective_receiver_port)
        else:
            self._runtime_receiver_port_override = None

        connData = self.json_dir_parser.json_from_path(conn_map_json)
        batch_size = int(dcData["nerlnetSettings"]["batchSize"])
        self.explicit_json_paths = (dc_json, conn_map_json, experiment_flow_json)

        globe.components = NetworkComponents(dcData) # move network component into experiment class
        globe.components.validate_connection_map(connData.get("connectionsMap", {}))
        # comDB = NerlComDB(globe.components)
        self.__new_experiment(experiment_name, experiment_flow_json, batch_size, globe.components, csv_path) # create new experiment
        self.experiment_focused_on(experiment_name)
        self.current_exp.reset_comm_stats_snapshot()
        # Reset phase cursor/flag for every new experiment initialization.
        self.current_exp.current_exp_phase_index = 0
        self.next_expertiment_phase_exist = True

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

        receiver_ip = str(globe.components.receiverIp)
        receiver_port = int(globe.components.receiverPort)

        # Initializing the receiver (a Flask HTTP server that receives results from the Main Server):
        if is_port_free(receiver_port, receiver_ip):
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
            self._receiver_bind = (receiver_ip, receiver_port)
        else:
            if hasattr(self, 'receiverThread') and self.receiverThread is not None and self.receiverThread.is_alive():
                LOG_INFO(
                    f"ApiServer receiver is already running on "
                    f"http://{receiver_ip}:{receiver_port}; reusing existing thread"
                )
                self._receiver_bind = (receiver_ip, receiver_port)
            else:
                LOG_ERROR(
                    f"ApiServer receiver port is already in use: "
                    f"http://{receiver_ip}:{receiver_port}. "
                    f"Stop the process using this port and retry."
                )
                raise RuntimeError(
                    f"ApiServer receiver port is busy at http://{receiver_ip}:{receiver_port}"
                )


        LOG_INFO("*** Remember to execute NerlnetRun.sh on each device before running the experiment! ***")
        
                
    def send_jsons_to_devices(self): #User Api
        archAddress , connMapAddress, _ = self.getUserJsons()
        if not archAddress or not connMapAddress:
            if self.explicit_json_paths:
                archAddress, connMapAddress, _ = self.explicit_json_paths
            else:
                raise RuntimeError("No distributed config/connection map were selected for transmission.")
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
            send_jsons_ack_timeout_sec = float(
                os.getenv("NERLNET_SEND_JSONS_ACK_TIMEOUT_SEC", "120")
            )
            try:
                self.apiserver_event_sync.sync_on_event(
                    EventSync.SEND_JSONS,
                    timeout_sec=send_jsons_ack_timeout_sec,
                    wait_label="Main Server sendJsons ack (received_jsons_done)"
                )
            except TimeoutError as exc:
                LOG_ERROR(
                    "Timed out waiting for Main Server sendJsons ack. "
                    "Likely causes: init handler crash, JSON parse failure, or disk space exhaustion "
                    "under /tmp/nerlnet or /tmp. "
                    "Check controller logs and run: `df -h /tmp /`, `du -sh /tmp/nerlnet`."
                )
                raise RuntimeError(str(exc)) from exc
            LOG_INFO("Sending distributed configurations to devices is completed")

    def _get_torch_assets(self):
        components = getattr(globe, 'components', None)
        if not components or not hasattr(components, 'get_torch_model_assets'):
            return {}
        return components.get_torch_model_assets()

    def _prepare_dc_stream(self, stack: ExitStack, dc_path: str, torch_assets: dict):
        has_runtime_receiver_override = self._runtime_receiver_port_override is not None
        if not torch_assets and not has_runtime_receiver_override:
            return stack.enter_context(open(dc_path, 'rb'))

        with open(dc_path, 'r', encoding='utf-8') as dc_file_obj:
            dc_dict = json.load(dc_file_obj)

        mutated = False
        if has_runtime_receiver_override:
            api_server_section = dc_dict.setdefault("apiServer", {})
            current_port = api_server_section.get("port")
            desired_port = self._runtime_receiver_port_override
            if str(current_port) != str(desired_port):
                if isinstance(current_port, str):
                    api_server_section["port"] = str(desired_port)
                else:
                    api_server_section["port"] = int(desired_port)
                mutated = True

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

    @staticmethod
    def _get_timeout_from_env(env_name: str, default_sec: float) -> float:
        raw_value = os.getenv(env_name, str(default_sec))
        try:
            timeout_sec = float(raw_value)
            if timeout_sec <= 0:
                raise ValueError("timeout must be > 0")
            return timeout_sec
        except (TypeError, ValueError):
            LOG_WARNING(
                "Invalid %s='%s'; using default %.1fs",
                env_name,
                raw_value,
                default_sec
            )
            return float(default_sec)

    @staticmethod
    def _get_bool_from_env(env_name: str, default: bool = False) -> bool:
        raw_value = str(os.getenv(env_name, str(int(default)))).strip().lower()
        if raw_value in ("1", "true", "yes", "on"):
            return True
        if raw_value in ("0", "false", "no", "off"):
            return False
        LOG_WARNING(
            "Invalid %s='%s'; using default %s",
            env_name,
            raw_value,
            default
        )
        return default

    @staticmethod
    def _dataset_csv_exists(path_to_repo: str) -> bool:
        if not os.path.isdir(path_to_repo):
            return False
        try:
            return any(file_name.endswith(".csv") for file_name in os.listdir(path_to_repo))
        except OSError:
            return False

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
        update_csv_timeout_sec = self._get_timeout_from_env("NERLNET_UPDATE_CSV_TIMEOUT_SEC", 180)
        events_sync_inst.sync_on_event(
            EventSync.UPDATE_CSV,
            timeout_sec=update_csv_timeout_sec,
            wait_label=f"{experiment_phase.get_name()} update_csv_done"
        )
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
            self.transmitter.clients_set_phase(
                current_exp_phase.get_phase_type(),
                current_exp_phase.get_parallel_execution()
            )
            update_phase_timeout_sec = self._get_timeout_from_env("NERLNET_UPDATE_PHASE_TIMEOUT_SEC", 180)
            events_sync_inst.sync_on_event(
                EventSync.UPDATE_PHASE,
                timeout_sec=update_phase_timeout_sec,
                wait_label=f"{current_exp_phase.get_name()} update_phase_done"
            )

            events_sync_inst.set_event_wait(EventSync.START_CASTING)
            self.transmitter.start_casting(current_exp_phase) # Source start sending data to workers
            start_casting_timeout_sec = self._get_timeout_from_env(
                "NERLNET_START_CASTING_TIMEOUT_SEC",
                1800
            )
            events_sync_inst.sync_on_event(
                EventSync.START_CASTING,
                timeout_sec=start_casting_timeout_sec,
                wait_label=f"{current_exp_phase.get_name()} start_casting_done"
            )

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
        phase_failures = []
        
        # Get the initial phase information
        current_exp_flow = self.current_exp
        total_phases = len(current_exp_flow.exp_phase_list)
        # Ensure phase state is reset for each full-flow execution.
        current_exp_flow.current_exp_phase_index = 0
        current_exp_flow.reset_comm_stats_snapshot()
        self.next_expertiment_phase_exist = True
        
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
                err_msg = str(e) if str(e) else repr(e)
                LOG_ERROR(f"Error running phase {phase_count}/{total_phases} '{phase_name}': {err_msg}")
                phase_failures.append((phase_name, phase_type, err_msg))
                # Continue with next phase instead of stopping completely
                
            # Move to next phase
            next_phase_type = self.next_experiment_phase()
            if next_phase_type is None:
                if phase_failures:
                    LOG_WARNING(
                        f"Experiment phases finished with failures. "
                        f"failed={len(phase_failures)} succeeded={len(all_phases_stats)} total={total_phases}"
                    )
                    for failed_phase_name, failed_phase_type, failed_reason in phase_failures:
                        LOG_WARNING(
                            f"Failed phase summary: name='{failed_phase_name}' "
                            f"type='{failed_phase_type}' reason='{failed_reason}'"
                        )
                else:
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
        except RepositoryNotFoundError:
            LOG_INFO(f"Failed to find the repository '{repo}'. Check your '{HF_DATA_REPO_PATHS_JSON}' file or network access.")
            
    def download_dataset(self, repo_idx : int, download_dir_path : str = DEFAULT_NERLNET_TMP_DATA_DIR):
        with open(HF_DATA_REPO_PATHS_JSON) as file:
            repo_ids = json.load(file)
        try:
            if isinstance(repo_idx, int):
                repo_idx = [repo_idx]
            force_refresh = self._get_bool_from_env("NERLNET_DATASET_REFRESH", False)
            offline_only = self._get_bool_from_env("NERLNET_DATASET_OFFLINE", False)
            for repo in repo_ids["datasets"]:
                if repo["idx"] in repo_idx:
                    repo_id = repo["id"]
                    full_path_to_repo = f'{download_dir_path}/{repo["name"]}'
                    if not os.path.exists(full_path_to_repo):
                        os.makedirs(full_path_to_repo)
                    local_csv_exists = self._dataset_csv_exists(full_path_to_repo)
                    if local_csv_exists and not force_refresh:
                        LOG_INFO(
                            f"Using cached dataset at {full_path_to_repo} "
                            f"(set NERLNET_DATASET_REFRESH=1 to re-download)"
                        )
                        continue
                    LOG_INFO(
                        f"Downloading dataset repo={repo_id} to {full_path_to_repo} "
                        f"(offline_only={offline_only}, force_refresh={force_refresh})"
                    )
                    snapshot_download(
                        repo_id=repo_id,
                        local_dir=f'{full_path_to_repo}',
                        repo_type="dataset",
                        allow_patterns=["*.csv"],
                        local_files_only=offline_only
                    )
                    LOG_INFO(f"Files downloaded to {download_dir_path}/{repo['name']}")
        except RepositoryNotFoundError:
            LOG_INFO(f"Failed to find the repository '{repo}'. Check your '{HF_DATA_REPO_PATHS_JSON}' file or network access.")
        
    
    def add_repo_to_datasets_list(self, repo_id , name : str = "" , description : str = ""):
        try:
            api = HfApi()
            api.list_repo_files(repo_id=repo_id , repo_type="dataset")
        except RepositoryNotFoundError:
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
