
import os
from experiment_flow_defs import *
from logger import *
from definitions import *
from NerlComDB import *
from nerl_model_db import *
from nerl_csv_dataset_db import *
from events_sync import *
from networkComponents import *
from stats import * 
from statsTiles import *
from stats_aec import *
from experiment_phase import *
from parallel_scheduler import (
    assert_scheduler_trace_integrity,
    build_scheduler_schedule,
    infer_stage_world_size,
)

PARAM_CSV_DB_PATH = "csv_db_path"
PARAM_BATCH_SIZE = "batch_size"

class ExperimentFlow():

    DATA_SOURCE_TYPE_CSV = 0
    DATA_SOURCE_TYPE_CAMERA = 1
    def __init__(self ,experiment_name, batch_size_dc: int, network_componenets: NetworkComponents, temp_data_path = NERLNET_TEMP_DATA_DIR, data_source_type = DATA_SOURCE_TYPE_CSV):
        self.exp_name = experiment_name
        self.exp_type = None
        self.batch_size_dc = batch_size_dc
        self.batch_size = None  # batch size from parsed exp_flow_json
        self.network_componenets = network_componenets
        self.temp_data_path = f"{temp_data_path}/{self.exp_name}"
        if not os.path.exists(self.temp_data_path):
            os.makedirs(self.temp_data_path)
        self.csv_dataset = None
        self.exp_phase_list = []
        self.current_exp_phase_index = 0
        self.exp_flow_json = None
        self.events_sync_inst = EventSync()

    def get_current_experiment_phase(self) -> ExperimentPhase:
        assert self.current_exp_phase_index < len(self.exp_phase_list) , "current experiment phase index is out of range"
        return self.exp_phase_list[self.current_exp_phase_index]

    def get_exp_name(self):
        return self.exp_name

    def get_exp_phase_list(self):
        return self.exp_phase_list
    
    def get_events_sync(self):
        return self.events_sync_inst
    
    def get_csv_dataset(self):
        return self.csv_dataset

    def generate_stats(self, experiment_phase = None) -> Stats:
        if experiment_phase is None:
            experiment_phase = self.get_current_experiment_phase() 
        return Stats(experiment_phase)
    
    def generate_stats_tiles(self, experiment_phase = None) -> StatsTiles:
        if experiment_phase is None:
            experiment_phase = self.get_current_experiment_phase() 
        return StatsTiles(experiment_phase)
    
    def generate_stats_aec(self, stats: Stats) -> Stats:
        assert stats is not None , "stats is None"
        return StatsAEC(stats)

    def merge_stats(self, stats_list: list) -> Stats:
        pass

    def parse_experiment_flow_json(self, json_path : str, override_csv_path = ""):
        '''
        json path is the path to the json file that was created by the nerlPlanner
        override_csv_path is the path to the csv file that will be used instead of the one in the json file
        if it is empty the csv file path from the json file will be used
        '''
        # read experimentFlow json file
        with open(json_path) as json_file:
            self.exp_flow_json = json.load(json_file)
        # parse json and create experiment phases
        self.exp_name = self.exp_flow_json[EXPFLOW_EXPERIMENT_NAME_FIELD]
        assert self.exp_flow_json[EXPFLOW_EXPERIMENT_TYPE_FIELD] , "experiment type is missing"
        self.exp_type = self.exp_flow_json[EXPFLOW_EXPERIMENT_TYPE_FIELD]
        self.batch_size = self.exp_flow_json[EXPFLOW_BATCH_SIZE_FIELD]
        assert self.batch_size == self.batch_size_dc, "Make sure the batch size field in the DC json and the Exp_flow json are the same"
        csv_file_path = self.exp_flow_json[EXPFLOW_CSV_FILE_PATH_FIELD] if override_csv_path == "" else override_csv_path
        headers_row = self.exp_flow_json[EXPFLOW_HEADERS_NAMES_FIELD].split(",")
        num_of_features = self.exp_flow_json[EXPFLOW_NUM_OF_FEATURES_FIELD]
        num_of_labels = self.exp_flow_json[EXPFLOW_NUM_OF_LABELS_FIELD]
        self.set_csv_dataset(csv_file_path, num_of_features, num_of_labels, headers_row)
        phases_list = self.exp_flow_json[EXPFLOW_PHASES_FIELD]
        phases_names_dict= {}
        phase_index = 1
        for phase in phases_list:
            phase_name = phase[EXPFLOW_PHASES_PHASE_NAME_FIELD]
            assert phase_name not in phases_names_dict , "check for duplicate phase names" 
            phases_names_dict.update({phase_name: phase_index})
            phase_index += 1
            phase_type = phase[EXPFLOW_PHASES_PHASE_TYPE_FIELD]
            sourcePieces = phase[EXPFLOW_PHASES_PHASE_SOURCE_PIECES_FIELD]
            parallel_execution = self._parse_parallel_execution(phase, sourcePieces)
            self._validate_pipeline_source_piece_workers(
                phase_name,
                parallel_execution.get(EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD, "legacy"),
                sourcePieces,
            )
            source_pieces_inst_list = []
            for source_piece in sourcePieces:
                # build source piece instant 
                source_name = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_SOURCE_NAME_FIELD]
                starting_sample = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_STARTING_SAMPLE_FIELD])
                num_of_batches = int(source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD])
                workers = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_WORKERS_FIELD]
                nerltensor_type = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NERLTENSOR_TYPE_FIELD]
                source_piece_inst = self.csv_dataset.generate_source_piece_ds(source_name, self.batch_size, phase_type, starting_sample, num_of_batches, nerltensor_type)
                source_piece_inst.update_target_workers(workers)
                source_piece_csv_file = self.csv_dataset.generate_source_piece_ds_csv_file(source_piece_inst, phase_type, phase_name)
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet(source_piece_csv_file)
                source_pieces_inst_list.append(source_piece_inst)
            LOG_INFO(f"phase {phase_name} source pieces parsed and generated.")
                
            self.add_phase(
                phase_name,
                phase_type,
                source_pieces_inst_list,
                num_of_features,
                parallel_execution
            )


    def set_csv_dataset(self, csv_file_path : str,  num_of_features : int, num_of_labels : int, headers_row : list):
        self.csv_dataset = CsvDataSet(csv_file_path, self.temp_data_path ,self.batch_size, num_of_features, num_of_labels, headers_row)  # Todo get num of features and labels from csv file

    def add_phase(
        self,
        name : str,
        phase_type : str,
        source_pieces_inst_list : list,
        num_of_features : str,
        parallel_execution = None
    ):
        exp_phase_inst = ExperimentPhase(
            self.exp_name,
            self.exp_type,
            name,
            phase_type,
            self.network_componenets,
            num_of_features,
            parallel_execution
        )
        for source_piece_inst in source_pieces_inst_list:
            exp_phase_inst.add_source_piece(source_piece_inst)
        self.exp_phase_list.append(exp_phase_inst)

    def _parse_parallel_execution(self, phase_dict: dict, source_pieces=None):
        raw_parallel = phase_dict.get(EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD, None)
        if raw_parallel is None:
            return {"mode": "legacy"}
        if not isinstance(raw_parallel, dict):
            raise ValueError(
                f"phase '{phase_dict.get(EXPFLOW_PHASES_PHASE_NAME_FIELD, '')}' has invalid "
                f"'{EXPFLOW_PHASES_PARALLEL_EXECUTION_FIELD}' (must be object)"
            )

        mode = str(raw_parallel.get(EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD, "legacy")).strip().lower()
        if mode not in EXPFLOW_PARALLEL_MODES:
            raise ValueError(
                f"invalid parallelExecution.mode '{mode}' in phase "
                f"'{phase_dict.get(EXPFLOW_PHASES_PHASE_NAME_FIELD, '')}'"
            )

        normalized = {
            EXPFLOW_PARALLEL_EXECUTION_MODE_FIELD: mode
        }
        phase_name = phase_dict.get(EXPFLOW_PHASES_PHASE_NAME_FIELD, "")
        if mode != "legacy":
            inferred_max_batches = self._infer_phase_max_batches(source_pieces, phase_name)
            configured_max_batches = raw_parallel.get(EXPFLOW_PARALLEL_EXECUTION_MAX_BATCHES_FIELD, None)
            if configured_max_batches is not None:
                configured_max_batches = self._parse_non_negative_int_field(
                    raw_parallel,
                    EXPFLOW_PARALLEL_EXECUTION_MAX_BATCHES_FIELD
                )
            if inferred_max_batches is not None:
                if configured_max_batches is not None and configured_max_batches != inferred_max_batches:
                    raise ValueError(
                        f"parallelExecution.{EXPFLOW_PARALLEL_EXECUTION_MAX_BATCHES_FIELD} ({configured_max_batches}) "
                        f"must match max source-piece numOfBatches ({inferred_max_batches}) in phase '{phase_name}'"
                    )
                normalized[EXPFLOW_PARALLEL_EXECUTION_MAX_BATCHES_FIELD] = inferred_max_batches
            elif configured_max_batches is not None:
                normalized[EXPFLOW_PARALLEL_EXECUTION_MAX_BATCHES_FIELD] = configured_max_batches

        super_node = str(raw_parallel.get(EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD, "")).strip()
        if mode != "legacy":
            if not super_node:
                raise ValueError(
                    f"parallel phase '{phase_name}' must define "
                    f"'{EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD}'"
                )
            if not self.network_componenets.has_super_nodes():
                raise ValueError("parallelExecution requires at least one configured super node")
            super_nodes = set(self.network_componenets.get_super_nodes_list())
            if super_node not in super_nodes:
                raise ValueError(
                    f"phase '{phase_name}' references unknown "
                    f"super node '{super_node}'"
                )
        if super_node:
            normalized[EXPFLOW_PARALLEL_EXECUTION_SUPER_NODE_FIELD] = super_node

        scheduler = str(raw_parallel.get(EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD, "")).strip().lower()
        if mode in ("pipeline", "pipeline_tensor"):
            if scheduler not in EXPFLOW_PARALLEL_SCHEDULERS:
                raise ValueError(
                    f"phase '{phase_name}' requires a valid "
                    f"pipeline scheduler in {sorted(EXPFLOW_PARALLEL_SCHEDULERS)}"
                )
            normalized[EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD] = scheduler
            normalized[EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD] = self._parse_positive_int_field(
                raw_parallel,
                EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD
            )
            normalized[EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD] = self._parse_positive_int_field(
                raw_parallel,
                EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD
            )
            total_microbatch_samples = (
                normalized[EXPFLOW_PARALLEL_EXECUTION_MICRO_BATCH_SIZE_FIELD]
                * normalized[EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD]
            )
            expected_phase_batch_size = int(
                self.batch_size if self.batch_size is not None else self.batch_size_dc
            )
            if total_microbatch_samples != expected_phase_batch_size:
                raise ValueError(
                    f"parallelExecution microBatchSize*numMicroBatches must equal phase batchSize "
                    f"({expected_phase_batch_size}), got {total_microbatch_samples}"
                )
            if scheduler == "interleaved":
                normalized[EXPFLOW_PARALLEL_EXECUTION_VIRTUAL_STAGES_FIELD] = self._parse_positive_int_field(
                    raw_parallel,
                    EXPFLOW_PARALLEL_EXECUTION_VIRTUAL_STAGES_FIELD
                )
            if not self.network_componenets.has_pipeline_workers():
                raise ValueError(
                    f"phase '{phase_name}' selected pipeline mode "
                    f"but no worker parallel pipeline metadata is configured"
                )

            stage_world_size = infer_stage_world_size(self.network_componenets.get_worker_parallel_map())
            if stage_world_size < 2:
                raise ValueError(
                    f"phase '{phase_name}' requires at least 2 pipeline stages"
                )

            if mode == "pipeline":
                phase_workers = self._extract_phase_target_workers(phase_dict)
                self._validate_pipeline_mode_worker_layout(
                    phase_name,
                    phase_workers,
                )

            virtual_stages = normalized.get(EXPFLOW_PARALLEL_EXECUTION_VIRTUAL_STAGES_FIELD, 1)
            schedule = build_scheduler_schedule(
                scheduler,
                stage_world_size,
                normalized[EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD],
                virtual_stages=virtual_stages
            )
            effective_stage_count = stage_world_size if scheduler != "interleaved" else stage_world_size * virtual_stages
            assert_scheduler_trace_integrity(
                schedule,
                effective_stage_count,
                normalized[EXPFLOW_PARALLEL_EXECUTION_NUM_MICRO_BATCHES_FIELD]
            )
            normalized["stageWorldSize"] = stage_world_size
            normalized["scheduleTraceLength"] = len(schedule)
        else:
            if scheduler:
                if scheduler not in EXPFLOW_PARALLEL_SCHEDULERS:
                    raise ValueError(
                        f"phase '{phase_name}' has invalid "
                        f"scheduler '{scheduler}'"
                )
                normalized[EXPFLOW_PARALLEL_EXECUTION_SCHEDULER_FIELD] = scheduler

        if mode in ("tensor", "pipeline_tensor") and not self.network_componenets.has_tp_workers():
            raise ValueError(
                f"phase '{phase_name}' selected tensor mode "
                f"but no worker TP metadata is configured"
            )

        return normalized

    def _extract_phase_target_workers(self, phase_dict: dict):
        workers = set()
        source_pieces = phase_dict.get(EXPFLOW_PHASES_PHASE_SOURCE_PIECES_FIELD, [])
        if not isinstance(source_pieces, list):
            return workers
        for source_piece in source_pieces:
            if not isinstance(source_piece, dict):
                continue
            for worker_name in self._extract_source_piece_workers(source_piece):
                workers.add(worker_name)
        return workers

    def _extract_source_piece_workers(self, source_piece: dict):
        raw_workers = source_piece.get(EXPFLOW_PHASE_SOURCE_PIECES_WORKERS_FIELD, "")
        if isinstance(raw_workers, str):
            return [worker_name.strip() for worker_name in raw_workers.split(",") if worker_name.strip()]
        if isinstance(raw_workers, list):
            return [str(worker_name).strip() for worker_name in raw_workers if str(worker_name).strip()]
        return []

    def _validate_pipeline_source_piece_workers(self, phase_name: str, phase_mode: str, source_pieces):
        if phase_mode not in ("pipeline", "pipeline_tensor"):
            return
        if not isinstance(source_pieces, list):
            return

        worker_parallel_map = self.network_componenets.get_worker_parallel_map()
        for source_piece in source_pieces:
            if not isinstance(source_piece, dict):
                continue
            source_name = source_piece.get(EXPFLOW_PHASE_SOURCE_PIECES_SOURCE_NAME_FIELD, "")
            source_workers = self._extract_source_piece_workers(source_piece)
            if not source_workers:
                raise ValueError(
                    f"phase '{phase_name}' source '{source_name}' must target at least one stage-0 worker "
                    f"when mode='{phase_mode}'"
                )
            for worker_name in source_workers:
                worker_parallel = worker_parallel_map.get(worker_name, {})
                stage = worker_parallel.get(PIPELINE_STAGE_FIELD)
                if stage is None:
                    raise ValueError(
                        f"phase '{phase_name}' source '{source_name}' targets worker '{worker_name}' "
                        f"without pipelineStage metadata. pipeline ingress must target stage 0 workers only"
                    )
                if int(stage) != 0:
                    raise ValueError(
                        f"phase '{phase_name}' source '{source_name}' targets worker '{worker_name}' "
                        f"at pipelineStage={stage}. only stage 0 workers can receive source batches in mode='{phase_mode}'"
                    )

    def _validate_pipeline_mode_worker_layout(self, phase_name: str, phase_workers: set):
        worker_parallel_map = self.network_componenets.get_worker_parallel_map()
        if not worker_parallel_map:
            return

        stage_to_workers = {}
        for worker_name, cfg in worker_parallel_map.items():
            if phase_workers and worker_name not in phase_workers:
                continue
            stage = cfg.get(PIPELINE_STAGE_FIELD)
            if stage is None:
                continue
            stage_to_workers.setdefault(stage, []).append(worker_name)

        invalid = {
            stage: sorted(workers)
            for stage, workers in stage_to_workers.items()
            if len(workers) != 1
        }
        if invalid:
            raise ValueError(
                f"phase '{phase_name}' in mode 'pipeline' currently requires exactly one worker "
                f"per pipeline stage. got stage->workers={invalid}. use mode 'pipeline_tensor' "
                f"for multi-worker stages."
            )

    def _parse_positive_int_field(self, parent_dict: dict, field_name: str):
        if field_name not in parent_dict:
            raise ValueError(f"Missing required parallelExecution field '{field_name}'")
        value = parent_dict[field_name]
        try:
            parsed = int(value)
        except (TypeError, ValueError) as exc:
            raise ValueError(f"parallelExecution field '{field_name}' must be an integer") from exc
        if parsed < 1:
            raise ValueError(f"parallelExecution field '{field_name}' must be >= 1")
        return parsed

    def _parse_non_negative_int_field(self, parent_dict: dict, field_name: str):
        if field_name not in parent_dict:
            raise ValueError(f"Missing required parallelExecution field '{field_name}'")
        value = parent_dict[field_name]
        try:
            parsed = int(value)
        except (TypeError, ValueError) as exc:
            raise ValueError(f"parallelExecution field '{field_name}' must be an integer") from exc
        if parsed < 0:
            raise ValueError(f"parallelExecution field '{field_name}' must be >= 0")
        return parsed

    def _infer_phase_max_batches(self, source_pieces, phase_name: str):
        if not isinstance(source_pieces, list):
            return None
        max_batches = None
        for source_piece in source_pieces:
            if not isinstance(source_piece, dict):
                continue
            if EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD not in source_piece:
                continue
            raw_batches = source_piece[EXPFLOW_PHASE_SOURCE_PIECES_NUM_OF_BATCHES_FIELD]
            try:
                parsed_batches = int(raw_batches)
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"phase '{phase_name}' has non-integer numOfBatches value '{raw_batches}'"
                ) from exc
            if parsed_batches < 0:
                raise ValueError(
                    f"phase '{phase_name}' has negative numOfBatches value '{parsed_batches}'"
                )
            max_batches = parsed_batches if max_batches is None else max(max_batches, parsed_batches)
        return max_batches

        
    def print(self):
        
        LOG_INFO(f"Experiment name: {self.exp_name}")
        LOG_INFO(f"Batch size: {self.batch_size}")
        #LOG_INFO(f"Temp data path: {self.temp_data_path}")
        #LOG_INFO(f"CSV dataset: {self.csv_dataset.get_csv_file_path()}")
        LOG_INFO(f"Number of features: {self.csv_dataset.get_num_of_features()}")
        LOG_INFO(f"Number of labels: {self.csv_dataset.get_num_of_labels()}")
        LOG_INFO("")
        LOG_INFO("Phases:")
        for phase in self.exp_phase_list:
            LOG_INFO(f"   Phase name: {phase.get_name()}")
            LOG_INFO(f"   Phase type: {phase.get_phase_type()}")
            LOG_INFO(f"   Parallel execution: {phase.get_parallel_execution()}")
            LOG_INFO(f"   Sources: {phase.get_sources_str_list()}")
            LOG_INFO("")
            LOG_INFO("    Source pieces:")
            for source_piece in phase.get_sources_pieces():
                LOG_INFO(f"         Source name: {source_piece.get_source_name()}")
                LOG_INFO(f"         Batch size: {source_piece.get_batch_size()}")
                LOG_INFO(f"         Phase: {source_piece.get_phase()}")
                LOG_INFO(f"         Starting offset: {source_piece.get_starting_offset()}")
                LOG_INFO(f"         Number of batches: {source_piece.get_num_of_batches()}")
                LOG_INFO(f"         Workers target: {source_piece.get_target_workers()}")
                LOG_INFO("      ----------------------")
                LOG_INFO("")
