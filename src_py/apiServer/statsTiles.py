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

    def __init__(self, experiment_phase: ExperimentPhase):
        super().__init__(experiment_phase)


    def get_confusion_matrices(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False): 
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments" 
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"   
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        confusion_matrix_source_dict = {}
        confusion_matrix_worker_dict = {}
        distributed_tokens_dict = {}
        worker_name_labels_dict={}
        for source_piece_inst in sources_pieces_list:
            sourcePiece_csv_labels_path = source_piece_inst.get_pointer_to_sourcePiece_CsvDataSet_labels()
            df_actual_labels = pd.read_csv(sourcePiece_csv_labels_path)
            num_of_labels = df_actual_labels.shape[1]
            header_list = range(num_of_labels) 
            df_actual_labels.columns = header_list
            df_actual_labels = self.expend_labels_df(df_actual_labels)
            source_name = source_piece_inst.get_source_name()
            target_workers = source_piece_inst.get_target_workers()
            worker_missed_batches = {}
            batch_size = source_piece_inst.get_batch_size()
            for worker_db in workers_model_db_list:
                worker_name = worker_db.get_worker_name()
                if worker_name not in target_workers:
                    continue
                df_worker_labels = df_actual_labels.copy()
                total_batches_per_source = worker_db.get_total_batches_per_source(source_name)
                print("total_batches_per_source: ", total_batches_per_source)
                for batch_id in range(total_batches_per_source):
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if not batch_db: # if batch is missing
                        if not self.missed_batches_warning_msg:
                            LOG_WARNING(f"missed batches")
                            self.missed_batches_warning_msg = True
                        starting_offset = source_piece_inst.get_starting_offset()
                        df_worker_labels.iloc[batch_id * batch_size: (batch_id + 1) * batch_size, num_of_labels:] = None # set the actual label to None for the predict labels in the df 
                        worker_missed_batches[(worker_name, source_name, str(batch_id))] = (starting_offset + batch_id * batch_size, batch_size)  # save the missing batch
                
                #df_worker_labels = df_worker_labels.dropna()
                for batch_id in range(total_batches_per_source):
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if batch_db:
                        distributed_token_db = batch_db.get_distributed_token()
                        cycle = int(batch_db.get_batch_id())
                        tensor_data = batch_db.get_tensor_data()
                        tensor_data = tensor_data.reshape(batch_size, num_of_labels)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = None # Fix an issue of pandas of incompatible dtype
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = tensor_data
                        try:
                            distributed_tokens_dict[distributed_token_db][worker_name].append(batch_id)
                        except: 
                            distributed_tokens_dict[distributed_token_db] =  {worker_name : [batch_id]}
                        print("df_worker_labels: ", df_worker_labels)
                    else:
                        cycle = int(batch_id)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = 0 
                # Take 2 list from the df, one for the actual labels and one for the predict labels to build the confusion matrix
                max_column_predict_index = df_worker_labels.iloc[:, num_of_labels:].idxmax(axis=1)
                max_column_predict_index = max_column_predict_index.tolist()
                max_column_predict_index = [int(predict_index) - num_of_labels for predict_index in max_column_predict_index] # fix the index to original labels index
                max_column_labels_index = df_worker_labels.iloc[:, :num_of_labels].idxmax(axis=1)
                max_column_labels_index = max_column_labels_index.tolist()
                worker_name_labels_dict[worker_name] = {"labels": [max_column_labels_index,max_column_predict_index]}
            
            
            for distributed_token_key in distributed_tokens_dict.keys():
                workers = distributed_tokens_dict[distributed_token_key].keys()
                distributed_token_arr = []
                batch_id_arr = []
                for worker in workers:
                   labels = worker_name_labels_dict[worker]["labels"]
                   distributed_token_arr.append(labels)
                   batch_id_arr.append(distributed_tokens_dict[distributed_token_key][worker].sort(key=lambda x: x[0]))
                 
                batch_id_dict = {}
                for i in range(len(batch_id_arr)):
                    for batch_id in batch_id_arr[i]:
                        try:
                            batch_id_dict[batch_id].append(i)
                        except:
                            batch_id_dict[batch_id] = [i]
               
                for class_index, class_name in enumerate(self.headers_list):
                    class_actual_list_full = []
                    class_actual_predict_full = []
                    for batch_id_val,worker_num_list in enumerate(batch_id_dict):
                        class_actual_list = []
                        class_predict_list = []
                        workers_list = [workers[i] for i in worker_num_list]
                        labels_list = [distributed_token_arr[i] for i in worker_num_list]
                        for worker_id,worker in enumerate(workers_list):
                            cycle = int(batch_id_val)
                            start_index = cycle * batch_size
                            end_index = (cycle + 1) * batch_size
                            class_actual_list  = [1 if label_num == class_index else 0 for label_num in labels_list[worker_id][0][start_index:end_index]]
                            try:
                                class_predict_list += [1 if label_num == class_index else 0 for label_num in labels_list[worker_id][1][start_index:end_index]]
                            except:
                                class_predict_list = [1 if label_num == class_index else 0 for label_num in labels_list[worker_id][1][start_index:end_index]]
                        
                        class_predict_list = [1 if (i>=len(workers_list)//2 or i == len(workers_list))  else 0 for i in class_predict_list]
                        class_actual_list_full.extend(class_actual_list) 
                        class_actual_predict_full.extend(class_predict_list) 
                    confusion_matrix = metrics.confusion_matrix(class_actual_list_full, class_actual_predict_full)  
                    try:
                        confusion_matrix_worker_dict[(distributed_token_key, class_name)] += confusion_matrix   
                    except:
                        confusion_matrix_worker_dict[(distributed_token_key, class_name)] = confusion_matrix 

                    
        if plot:
            workers = sorted(list({tup[0] for tup in confusion_matrix_worker_dict.keys()}))
            classes = sorted(list({tup[1] for tup in confusion_matrix_worker_dict.keys()}))
            fig, ax = plt.subplots(nrows=len(workers), ncols=len(classes),figsize=(4*len(classes),4*len(workers)),dpi=140)
            for i , worker in enumerate(workers): 
                for j , pred_class in enumerate(classes):
                    conf_mat = confusion_matrix_worker_dict[(worker , pred_class)]
                    heatmap = sns.heatmap(data=conf_mat ,ax=ax[i,j], annot=True , fmt="d", cmap='Blues',annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                    cbar = heatmap.collections[0].colorbar
                    cbar.ax.tick_params(labelsize = 8)
                    ax[i, j].set_title(f"{worker} , Class '{pred_class}'" , fontsize=12)
                    ax[i, j].tick_params(axis='both', which='major', labelsize=8) 
                    ax[i, j].set_xlabel("Predicted Label" , fontsize=8)
                    ax[i, j].set_ylabel("True Label" , fontsize=8)
                    ax[i, j].set_aspect('equal')
            fig.subplots_adjust(wspace=0.4 , hspace=0.4)
            plt.show()
                
        

        return confusion_matrix_source_dict, confusion_matrix_worker_dict
                           