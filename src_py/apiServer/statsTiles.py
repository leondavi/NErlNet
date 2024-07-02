from IPython.display import display
import matplotlib.pyplot as plt
from datetime import datetime
from pathlib import Path
from experiment_phase import *
import globalVars as globe
from definitions import *
import pandas as pd
import numpy as np
import seaborn as sns
from stats import *

sns.set_theme()

class StatsTiles(Stats):

    def __init__(self):
        pass

    def get_confusion_matrices(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False): 
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments" 
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"   
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        confusion_matrix_source_dict = {}
        confusion_matrix_worker_dict = {}
        for source_piece_inst in sources_pieces_list:
            sourcePiece_csv_labels_path = source_piece_inst.get_pointer_to_sourcePiece_CsvDataSet_labels()
            df_actual_labels = pd.read_csv(sourcePiece_csv_labels_path)
            print(df_actual_labels,"df_actual_labels read_csv")
            num_of_labels = df_actual_labels.shape[1]
            print(num_of_labels,"num_of_labels")
            header_list = range(num_of_labels) 
            df_actual_labels.columns = header_list
            df_actual_labels = self.expend_labels_df(df_actual_labels)
            print(df_actual_labels,"df_actual_labels")
            #print(df_actual_labels)
            source_name = source_piece_inst.get_source_name()
            print(source_name,"source_name")
            # build confusion matrix for each worker
            target_workers = source_piece_inst.get_target_workers()
            print(target_workers,"target_workers")
            batch_size = source_piece_inst.get_batch_size()
            print(batch_size,"batch_size")
            for worker_db in workers_model_db_list:
                worker_name = worker_db.get_worker_name()
                print(worker_name,"worker_name")
                total_batches_per_source = worker_db.get_total_batches_per_source(source_name)
                print(total_batches_per_source,"total_batches_per_source")
        

        return confusion_matrix_source_dict, confusion_matrix_worker_dict
    