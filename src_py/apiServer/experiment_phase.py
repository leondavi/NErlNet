from NerlComDB import *
from nerl_model_db import *
from NerlPerfDB import *
from definitions import *
from nerl_csv_dataset_db import *
from decoderHttpMainServer import *

class ExperimentPhase():
    def __init__(self, experiment_flow_name : str, experiment_flow_type: str, name : str, phase_type: str, network_components: NetworkComponents, num_of_features: str):
        self.experiment_flow_name = experiment_flow_name
        self.experiment_flow_type = experiment_flow_type
        self.name = name 
        self.phase_type = phase_type # training/prediction
        assert self.phase_type in [PHASE_TRAINING_STR, PHASE_PREDICTION_STR]
        self.nerl_comm_db = NerlComDB(network_components)  
        self.nerl_perf_db = NerlPerfDB(network_components)
        self.nerl_model_db = NerlModelDB(self.phase_type)
        self.source_pieces_dict = {}  # Dict of SourcePieceDS 
        self.num_of_features = num_of_features
        self.raw_data_buffer = []
        self.network_components = network_components

    def get_raw_data_buffer(self):
        return self.raw_data_buffer

    def clean_raw_data_buffer(self):
        self.raw_data_buffer = []

    def process_experiment_phase_data(self):
        assert len(self.raw_data_buffer) == 1, "Expecting only one raw_data in buffer of a single phase"
        list_of_decoded_data = decode_phase_result_data_json_from_main_server(self.raw_data_buffer[0])
        for decoded_data in list_of_decoded_data:
            worker_name, source_name, duration, batch_id, batch_ts, distributed_token, np_tensor = decoded_data
            client_name = self.network_components.get_client_name_by_worker_name(worker_name)
            self.nerl_model_db.get_client(client_name).get_worker(worker_name).create_batch(batch_id, source_name, np_tensor, duration, distributed_token, batch_ts)
        
        self.clean_raw_data_buffer()

    def get_phase_type(self):
        return self.phase_type

    def get_name(self):
        return self.name
    
    def get_experiment_name(self):
        return self.experiment_flow_name
    
    def get_experiment_flow_name(self):
        return self.experiment_flow_name
    
    def get_experiment_flow_type(self):
        return self.experiment_flow_type

    def get_network_components(self):
        return self.network_components

    def get_sources_str_list(self):
        return ",".join(self.source_pieces_dict.keys())
    
    def get_nerl_comm_db(self):
        return self.nerl_comm_db
    
    def get_nerl_perf_db(self):
        return self.nerl_perf_db

    def get_nerl_model_db(self):
        return self.nerl_model_db

    def get_num_of_features(self):
        return self.num_of_features
    
    def add_source_piece(self, source_piece : SourcePieceDS):
        if source_piece.source_name not in self.source_pieces_dict:
            self.source_pieces_dict[source_piece.source_name] = source_piece
        else: 
            LOG_ERROR(f"Source piece with name {source_piece.source_name} already exists in phase { self.phase}")
    
    def get_sources_pieces(self):
        return list(self.source_pieces_dict.values())

    def remove_source_piece(self, source_name: str): 
        self.source_pieces_dict.pop(source_name)   
        
    def get_source_piece(self, source_name: str):
        return self.source_pieces_dict[source_name]
    