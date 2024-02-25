from NerlComDB import *
from nerl_model_db import *
from definitions import *
from nerl_csv_dataset_db import *

class ExperimentPhase():
    def __init__(self, experiment_flow_name : str, name : str, phase_type: str, network_componenets: NetworkComponents):
        self.experiment_flow_name = experiment_flow_name
        self.name = name 
        self.phase_type = phase_type # training/prediction
        assert self.phase_type in [PHASE_TRAINING_STR, PHASE_PREDICTION_STR]
        self.nerl_comm_db = NerlComDB(network_componenets)  
        self.nerl_model_db = NerlModelDB(self.phase_type)
        self.source_pieces_dict = {}  # Dict of SourcePieceDS

    def get_phase_type(self):
        return self.phase_type

    def get_name(self):
        return self.name
    
    def get_experiment_flow_name(self):
        return self.experiment_flow_name

    def get_sources_str_list(self):
        return ",".join(self.source_pieces_dict.keys())
    
    def get_nerl_comm_db(self):
        return self.nerl_comm_db

    def get_nerl_model_db(self):
        return self.nerl_model_db
    
    def add_source_piece(self, source_piece : SourcePieceDS):
        if source_piece.source_name not in self.source_pieces_dict:
            self.source_pieces_dict[source_piece.source_name] = source_piece
        else: 
            LOG_ERROR(f"Source piece with name {source_piece.source_name} already exists in phase { self.phase}")
       
    def get_sources_pieces(self):
        return list(self.source_pieces_dict.values())

    def remove_source_piece(self, source_name: str): 
        self.source_pieces_dict.pop(source_name)   