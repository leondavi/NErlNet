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
        confusion_matrix_source_dict, confusion_matrix_distributed_dict = self.get_confusion_matrices_tiles(normalize ,plot ,saveToFile )
        return confusion_matrix_source_dict, confusion_matrix_distributed_dict

    
    """
        This function is used to calculate the confusion matrix for each cluster and each class in the distributed tokens
        attention! get_confusion_matrices_tiles take the sum of the labels and from that do majority vote and
        then decide 0 or 1 for each class
    """
    def get_confusion_matrices_tiles(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False): 
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments" 
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"   
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        confusion_matrix_source_dict = {}
        confusion_matrix_distributed_dict = {}
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
            source_name_new = source_name.split("_")[1]
            target_workers = source_piece_inst.get_target_workers()
            worker_missed_batches = {}
            batch_size = source_piece_inst.get_batch_size()
            for worker_db in workers_model_db_list:
                worker_name = worker_db.get_worker_name()
                if worker_name not in target_workers:
                    continue
                df_worker_labels = df_actual_labels.copy()
                total_batches_per_source = worker_db.get_total_batches_per_source(source_name)
                for batch_id in range(total_batches_per_source):
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if not batch_db: # if batch is missing
                        starting_offset = source_piece_inst.get_starting_offset()
                        df_worker_labels.iloc[batch_id * batch_size: (batch_id + 1) * batch_size, num_of_labels:] = None # set the actual label to None for the predict labels in the df 
                        worker_missed_batches[(worker_name, source_name, str(batch_id))] = (starting_offset + batch_id * batch_size, batch_size)  # save the missing batch
                
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
                            distributed_dict = distributed_tokens_dict[distributed_token_db]
                            try:
                                batch_id_arr_worker = distributed_dict[worker_name]
                                batch_id_arr_worker.append((source_name_new,batch_id))
                                distributed_tokens_dict[distributed_token_db][worker_name] = batch_id_arr_worker
                            except: 
                                distributed_tokens_dict[distributed_token_db].update({worker_name : [(source_name_new,batch_id)]})
                        except:
                            distributed_tokens_dict[distributed_token_db] =  {worker_name : [(source_name_new,batch_id)]}
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
                try:
                    worker_name_labels_dict[source_name_new].update({worker_name : {"labels": [max_column_labels_index,max_column_predict_index]}})
                except:
                    worker_name_labels_dict[source_name_new]= {worker_name : {"labels": [max_column_labels_index,max_column_predict_index]}}
                
            
            
        for distributed_token_key in list(distributed_tokens_dict.keys()):
            workers = list(distributed_tokens_dict[distributed_token_key].keys())
            distributed_token_dict = {}
            batch_id_source_dict = {}
            for worker in list(workers):
                batch_source_list_for_worker = distributed_tokens_dict[distributed_token_key][worker]
                for value in batch_source_list_for_worker:
                    source_name = value[0]
                    batch_id = value[1]
                    labels = worker_name_labels_dict[source_name][worker]["labels"]
                    try: 
                        distributed_token_dict[source_name][worker] = labels
                    except:
                        distributed_token_dict[source_name] = {worker : labels}
                    try:
                        batch_id_source_dict[source_name][batch_id].append(worker)
                    except:
                        try:
                            batch_id_source_dict[source_name].update({batch_id : [worker]})
                        except:
                            batch_id_source_dict[source_name] = {batch_id : [worker]}

            for class_index, class_name in enumerate(self.headers_list):
                class_actual_list_full = []
                class_actual_predict_full = []
                for source_name in list(batch_id_source_dict.keys()):
                    batch_id_dict = batch_id_source_dict[source_name]
                    distributed_token_dict_temp = distributed_token_dict[source_name]
                    for batch_id_val in list(batch_id_dict.keys()):
                        class_actual_list = []
                        class_predict_list = []
                        workers_list = batch_id_dict[batch_id_val]
                        labels_dict = distributed_token_dict_temp
                        cycle = int(batch_id_val)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        for worker in workers_list:
                            class_actual_list  = [1 if label_num == class_index else 0 for label_num in labels_dict[worker][0][start_index:end_index]]
                            label_list_worker = labels_dict[worker][1][start_index:end_index]
                            class_predict_temp = [1 if label_num == class_index else 0 for label_num in label_list_worker]
                            if(class_predict_list == []):
                                class_predict_list = [0]*len(class_predict_temp)
                            try:
                                temp_list = [class_predict_list,class_predict_temp]
                                class_predict_list = [sum(x) for x in zip(*temp_list)]
                            except:
                                class_predict_list = [1 if label_num == class_index else 0 for label_num in label_list_worker]
                        class_predict_list_new = [1 if (i>len(workers_list)/2 or i == len(workers_list))  else 0 for i in class_predict_list]
                        class_actual_list_full.extend(class_actual_list) 
                        class_actual_predict_full.extend(class_predict_list_new) 
                    confusion_matrix = metrics.confusion_matrix(class_actual_list_full, class_actual_predict_full)  
                    try:
                        confusion_matrix_distributed_dict[(distributed_token_key, class_name)] += confusion_matrix   
                    except:
                        confusion_matrix_distributed_dict[(distributed_token_key, class_name)] = confusion_matrix 
        if plot:
            self.print_plot(confusion_matrix_distributed_dict) 
        return confusion_matrix_source_dict, confusion_matrix_distributed_dict
    
    def print_plot(self, confusion_matrix):
        distributed_token_worker = sorted(list({tup[0] for tup in confusion_matrix.keys()}))
        classes = sorted(list({tup[1] for tup in confusion_matrix.keys()}))
        fig, ax = plt.subplots(nrows=len(distributed_token_worker), ncols=len(classes),figsize=(4*len(classes),4*len(distributed_token_worker)),dpi=140)
        for i , token in enumerate(distributed_token_worker): 
            for j , pred_class in enumerate(classes):
                conf_mat = confusion_matrix[(token , pred_class)]
                heatmap = sns.heatmap(data=conf_mat ,ax=ax[i,j], annot=True , fmt="d", cmap='Blues',annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                cbar = heatmap.collections[0].colorbar
                cbar.ax.tick_params(labelsize = 8)
                ax[i, j].set_title(f"{token} , Class '{pred_class}'" , fontsize=12)
                ax[i, j].tick_params(axis='both', which='major', labelsize=8) 
                ax[i, j].set_xlabel("Predicted Label" , fontsize=8)
                ax[i, j].set_ylabel("True Label" , fontsize=8)
                ax[i, j].set_aspect('equal')
        fig.subplots_adjust(wspace=0.4 , hspace=0.4)
        plt.show()

    def add_lists_of_lists(self,list1, list2):
        if len(list1) != len(list2):
            raise ValueError("Both lists of lists must have the same number of sublists")
        result = [
            [item1 + item2 for item1, item2 in zip(sublist1, sublist2)]
         for sublist1, sublist2 in zip(list1, list2)
        ]
        return result

    def argmax_axis_1(self,list_of_lists):
    # Get the index of the maximum value in each sublist
        result = [sublist.index(max(sublist)) for sublist in list_of_lists]
        return result

    """
        This function is used to calculate the confusion matrix for each cluster and each class in the distributed tokens
        attention! get_confusion_matrices_tiles_new take the sum of the probabilities and from that do majority vote and
        then decide 0 or 1 for each class

    """
    def get_confusion_matrices_tiles_new(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False): 
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments" 
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"   
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        confusion_matrix_source_dict = {}
        confusion_matrix_distributed_dict = {}
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
            source_name_new = source_name.split("_")[1]
            target_workers = source_piece_inst.get_target_workers()
            worker_missed_batches = {}
            batch_size = source_piece_inst.get_batch_size()
            for worker_db in workers_model_db_list:
                worker_name = worker_db.get_worker_name()
                if worker_name not in target_workers:
                    continue
                df_worker_labels = df_actual_labels.copy()
                total_batches_per_source = worker_db.get_total_batches_per_source(source_name)
                for batch_id in range(total_batches_per_source):
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if not batch_db: # if batch is missing
                        starting_offset = source_piece_inst.get_starting_offset()
                        df_worker_labels.iloc[batch_id * batch_size: (batch_id + 1) * batch_size, num_of_labels:] = None # set the actual label to None for the predict labels in the df 
                        worker_missed_batches[(worker_name, source_name, str(batch_id))] = (starting_offset + batch_id * batch_size, batch_size)  # save the missing batch
                
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
                            distributed_dict = distributed_tokens_dict[distributed_token_db]
                            try:
                                batch_id_arr_worker = distributed_dict[worker_name]
                                batch_id_arr_worker.append((source_name_new,batch_id))
                                distributed_tokens_dict[distributed_token_db][worker_name] = batch_id_arr_worker
                            except: 
                                distributed_tokens_dict[distributed_token_db].update({worker_name : [(source_name_new,batch_id)]})
                        except:
                            distributed_tokens_dict[distributed_token_db] =  {worker_name : [(source_name_new,batch_id)]}
                    else:
                        cycle = int(batch_id)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = 0 
                # Take 2 list from the df, one for the actual labels and one for the predict labels to build the confusion matrix
                max_column_predict_index = df_worker_labels.iloc[:, num_of_labels:]
                max_column_predict_index = max_column_predict_index
                max_column_labels_index = df_worker_labels.iloc[:, :num_of_labels].idxmax(axis=1)
                max_column_labels_index = max_column_labels_index.tolist()
                try:
                    worker_name_labels_dict[source_name_new].update({worker_name : {"labels": [max_column_labels_index,max_column_predict_index]}})
                except:
                    worker_name_labels_dict[source_name_new]= {worker_name : {"labels": [max_column_labels_index,max_column_predict_index]}}
                
            
            
        for distributed_token_key in list(distributed_tokens_dict.keys()):
            workers = list(distributed_tokens_dict[distributed_token_key].keys())
            distributed_token_dict = {}
            batch_dict_worker_labels = {}
            batch_id_source_dict = {}
            for worker in list(workers):
                batch_source_list_for_worker = distributed_tokens_dict[distributed_token_key][worker]
                for value in batch_source_list_for_worker:
                    source_name = value[0]
                    batch_id = value[1]
                    labels = worker_name_labels_dict[source_name][worker]["labels"]
                    try:
                        batch_dict_worker_labels[source_name].update({batch_id: labels[0]})
                    except:
                        batch_dict_worker_labels[source_name] = {batch_id: labels[0]}
                    try: 
                        distributed_token_dict[source_name][worker] = labels
                    except:
                        distributed_token_dict[source_name] = {worker : labels}
                    try:
                        batch_id_source_dict[source_name][batch_id].append(worker)
                    except:
                        try:
                            batch_id_source_dict[source_name].update({batch_id : [worker]})
                        except:
                            batch_id_source_dict[source_name] = {batch_id : [worker]}
                   
                
            class_actual_predict_dict = {}
            for source_name in list(batch_id_source_dict.keys()):
                batch_id_dict = batch_id_source_dict[source_name]
                for batch_id_val in list(batch_id_dict.keys()):
                    class_predict_list = []
                    workers_list = batch_id_dict[batch_id_val]
                    labels_list = distributed_token_dict[source_name]
                    cycle = int(batch_id_val)
                    start_index = cycle * batch_size
                    end_index = (cycle + 1) * batch_size
                    for worker in workers_list:
                        label_list_worker = labels_list[worker][1][start_index:end_index]
                        class_predict_temp = label_list_worker.values.tolist()
                        if(class_predict_list == []):
                            class_predict_list = class_predict_temp
                        else:
                            class_predict_list = self.add_lists_of_lists(class_predict_list, class_predict_temp)
                    try:
                        class_actual_predict_dict[source_name].update({batch_id_val: class_predict_list})
                    except:
                         class_actual_predict_dict[source_name] = {batch_id_val: class_predict_list}

          
            for class_index, class_name in enumerate(self.headers_list):
                class_actual_predict_full = []
                class_actual_list_full = []
                for source_name in list(batch_id_source_dict.keys()):
                    batch_id_dict = batch_id_source_dict[source_name]
                    for batch_id_val in list(batch_id_dict.keys()):
                        cycle = int(batch_id_val)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        class_predict_list_prev = class_actual_predict_dict[source_name][batch_id_val]
                        class_predict_list = []
                        class_actual_list  = [1 if label_num == class_index else 0 for label_num in batch_dict_worker_labels[source_name][batch_id_val][start_index:end_index]]
                        max_column_predict_index = self.argmax_axis_1(class_predict_list_prev)
                        max_column_predict_index = [int(predict_index) for predict_index in max_column_predict_index] 
                        class_predict_list = [1 if label_num == class_index else 0 for label_num in max_column_predict_index]
                        class_actual_predict_full.extend(class_predict_list) 
                        class_actual_list_full.extend(class_actual_list) 

                confusion_matrix = metrics.confusion_matrix(class_actual_list_full, class_actual_predict_full)  
                try:
                    confusion_matrix_distributed_dict[(distributed_token_key, class_name)] += confusion_matrix   
                except:
                    confusion_matrix_distributed_dict[(distributed_token_key, class_name)] = confusion_matrix

        if plot:
            self.print_plot(confusion_matrix_distributed_dict)
        return confusion_matrix_source_dict, confusion_matrix_distributed_dict

    def get_model_performence_stats_tiles(self , confusion_matrix_worker_dict , show : bool = False , saveToFile : bool = False, printStats = False) -> dict:
        """
        Returns a dictionary of {(worker, class): {Performence_Stat : VALUE}}} for each worker and class in the experiment.
        Performence Statistics Available are: TN, FP, FN, TP, Accuracy, Balanced Accuracy, Precision, Recall, True Negative Rate, Informedness, F1
        """
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments"
        distributed_token_performence = OrderedDict()
        for (token_name, class_name) in confusion_matrix_worker_dict.keys():
            distributed_token_performence[(token_name, class_name)] = OrderedDict()
            tn, fp, fn, tp = confusion_matrix_worker_dict[(token_name, class_name)].ravel()
            if printStats:
                LOG_INFO(f"distributed token {token_name} class: {class_name} tn: {tn}, fp: {fp}, fn: {fn}, tp: {tp}")
            tn = int(tn)
            fp = int(fp)
            fn = int(fn)
            tp = int(tp)
            acc = (tp + tn) / (tp + tn + fp + fn)
            ppv = tp / (tp + fp) if tp > 0 else 0 # Precision
            tpr = tp / (tp + fn) if tp > 0 else 0 # Recall 
            tnr = tn / (tn + fp) if tn > 0 else 0
            bacc = (tpr + tnr) / 2
            inf = tpr + tnr - 1
            f1 = 2 * (ppv * tpr) / (ppv + tpr) if (ppv + tpr) > 0 else 0 # F1-Score

            distributed_token_performence[(token_name, class_name)]['TN'] = tn
            distributed_token_performence[(token_name, class_name)]['FP'] = fp
            distributed_token_performence[(token_name, class_name)]['FN'] = fn
            distributed_token_performence[(token_name, class_name)]['TP'] = tp
            distributed_token_performence[(token_name, class_name)]['Accuracy'] = acc
            distributed_token_performence[(token_name, class_name)]['Balanced Accuracy'] = bacc
            distributed_token_performence[(token_name, class_name)]['Precision'] = ppv
            distributed_token_performence[(token_name, class_name)]['Recall'] = tpr
            distributed_token_performence[(token_name, class_name)]['True Negative Rate'] = tnr
            distributed_token_performence[(token_name, class_name)]['Informedness'] = inf
            distributed_token_performence[(token_name, class_name)]['F1'] = f1
            
        df = pd.DataFrame.from_dict(distributed_token_performence, orient='index')
        stats = list(df.columns)
        df.reset_index(inplace=True)
        df.columns = ['Cluster', 'Class'] + stats
        if show:
            centered_df = df.style.set_properties(**{'text-align': 'center'}).set_table_styles([ # Center all 
                                {'selector': 'th', 'props': [('text-align', 'center')]},
                                {'selector': 'th.col_heading', 'props': [('text-align', 'center')]},
                                {'selector': 'th.row_heading', 'props': [('text-align', 'center')]}
                            ])
            display(centered_df)
        
        if saveToFile:
            LOG_INFO(f"Saving model performence stats to csv file: {EXPERIMENT_RESULTS_PATH}/{self.exp_path}/{MODEL_PERFORMANCE_FILENAME}")
            export_df_csv(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/{MODEL_PERFORMANCE_FILENAME}', df)
            
        return df


                  