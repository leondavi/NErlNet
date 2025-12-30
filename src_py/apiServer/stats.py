from collections import OrderedDict
import comm
from sklearn import metrics
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
sns.set_theme()
MIN_LOSS_BASELINE_FILENAME = "min_loss_dict.json"
MODEL_PERFORMANCE_FILENAME = "model_perf.csv"

MATRIX_DISP_SCALING = 5
class Stats():

    def __init__(self, experiment_phase: ExperimentPhase):
        self.experiment_phase = experiment_phase
        self.nerl_model_db = self.experiment_phase.get_nerl_model_db()
        self.nerl_comm_db = self.experiment_phase.get_nerl_comm_db()
        self.nerl_perf_db = self.experiment_phase.get_nerl_perf_db()
        self.phase = self.experiment_phase.get_phase_type()
        self.name = self.experiment_phase.get_experiment_name()
        self.net_comps = self.experiment_phase.get_network_components()
        self.workers_list = self.net_comps.get_workers_list()
        self.freq = self.net_comps.get_freq()
        self.batch_size = self.net_comps.get_batch_size()
        self.num_of_sources = self.net_comps.get_num_of_sources()
        self.sources_epochs_dict = self.net_comps.get_source_epochs_dict()
        self.loss_ts_pd = None
        self.missed_batches_warning_msg = False
        self.experiment_flow_type = self.experiment_phase.get_experiment_flow_type()
        if (self.phase == PHASE_PREDICTION_STR):
            for source_piece_inst in self.experiment_phase.get_sources_pieces():
                csv_dataset_inst = source_piece_inst.get_csv_dataset_parent()   # get the csv dataset instance (csv_dataset_db.py)
                source_piece_csv_labels_file = csv_dataset_inst.genrate_source_piece_ds_csv_file_labels(source_piece_inst, self.phase)  #return the path of the csv file that contains the source piece labels data
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet_labels(source_piece_csv_labels_file)
            self.headers_list = csv_dataset_inst.get_headers_row()

        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment_phase.get_experiment_flow_name()}').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment_phase.get_phase_type()}/{self.experiment_phase.get_experiment_flow_name()}').mkdir(parents=True, exist_ok=True)
        self.exp_path = f'{self.experiment_phase.get_experiment_flow_name()}_{self.name}_{self.phase}_{datetime.now().strftime("%Y_%m_%d_%H_%M_%S")}'

    def get_phase(self):
        return self.phase
    
    def get_name(self):
        return self.name
    
       # TODO 
    def get_loss_foh_missing(self , plot : bool = False , saveToFile : bool = False): # foh = first order hold - do avg between the existing points
        assert self.phase == PHASE_TRAINING_STR, "This function is only available for training phase"
        pass

    #TODO Implement this function
    def get_loss_by_source(self , plot : bool = False , saveToFile : bool = False): 
        """
        Returns a dictionary of {source : DataFrame[BatchID,'w1','w2'...'wn']} for each source in the experiment.
        """
        pass

    def get_loss_ts(self , plot : bool = False , saveToFile : bool = False, smoothing : bool = False, log_plot : bool = False): 
        """
        Returns a dictionary of {worker : loss list} for each worker in the experiment.
        use plot=True to plot the loss function.
        """
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments"
        assert self.phase == PHASE_TRAINING_STR, "This function is only available for training phase"
        loss_dict = {}
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        workers_names = [worker_model_db.get_worker_name() for worker_model_db in workers_model_db_list]
        num_of_workers = len(workers_model_db_list)
        total_batches_list = [worker.get_total_batches() for worker in workers_model_db_list]
        max_batches = max(total_batches_list)
        
        for worker_db in workers_model_db_list:
            worker_name = worker_db.get_worker_name()
            loss_dict[worker_name] = []
            batches_ts_tensor_data_dict =worker_db.get_batches_ts_tensor_data_dict()
            sorted_batches_ts_tensor_data_dict = dict(sorted(batches_ts_tensor_data_dict.items()))
            loss_dict[worker_name] = [sorted_batches_ts_tensor_data_dict[key] for key in sorted(sorted_batches_ts_tensor_data_dict)]
            
        # Convert NumPy arrays to floats
        for worker_name in loss_dict:
            flattened = []
            for item in loss_dict[worker_name]:
                if isinstance(item, np.ndarray):
                    # Flatten the array and extract all scalar values
                    flattened.extend(item.flatten().tolist())
                elif isinstance(item, (list, tuple)):
                    # If it's a nested structure, recursively flatten
                    for subitem in item:
                        if isinstance(subitem, np.ndarray):
                            flattened.extend(subitem.flatten().tolist())
                        else:
                            flattened.append(float(subitem))
                else:
                    flattened.append(float(item))
            loss_dict[worker_name] = flattened
            loss_dict[worker_name] += [None] * (max_batches - len(loss_dict[worker_name]))

        df = pd.DataFrame(loss_dict)
        self.loss_ts_pd = df

        if smoothing:
            for column in df.columns:
                for i in range(1, len(df)):
                    df.at[i, column] = (df.at[i, column] + df.at[i-1, column]) / 2      
        
        if plot:
            sns.set(style="whitegrid")
            plt.figure(figsize=(12, 8))
            plt.gca().set_facecolor('lightblue')  # Set the background color to light blue
            
            # Customize the grid lines to be black
            plt.grid(color='black', linestyle='-', linewidth=0.5)
            
            sns.lineplot(data=df)
            plt.xlabel('Batch Num.')
            plt.ylabel('Loss Value')
            if log_plot:
                plt.yscale('log')
                plt.xscale('log')
            plt.title(f'Training Loss Function of ({self.experiment_phase.get_name()})')

            # Move legend outside of the plot
            plt.legend(title="Worker", loc='center left', bbox_to_anchor=(1, 0.5), ncol=1)
            
            if saveToFile:
                plt.savefig('training_loss_function.png', bbox_inches='tight')

            if log_plot:
                plt.yscale('log')
            
            plt.show()
        return df

    def get_min_loss(self , plot : bool = False , saveToFile : bool = False): # Todo change it
        """
        Returns a dictionary of {worker : min loss} for each worker in the experiment.
        use plot=True to plot the min loss of each worker.
        """
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments"
        min_loss_dict = OrderedDict()
        if self.loss_ts_pd is None:
            loss_ts_pd = self.get_loss_ts()
        else:
            loss_ts_pd = self.loss_ts_pd
        for worker_name in loss_ts_pd.columns:
            min_loss = loss_ts_pd[worker_name].min(numeric_only=True)
            min_loss_dict[worker_name] = min_loss

        if saveToFile:
            LOG_INFO(f"Saving min loss dict to file: {EXPERIMENT_RESULTS_PATH}/{self.exp_path}/min_loss_dict.json")
            export_dict_json(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/{MIN_LOSS_BASELINE_FILENAME}', min_loss_dict)
            
        if plot:
            sns.barplot(data=min_loss_dict , palette='viridis')
            plt.ylabel('Min Loss Value')
            plt.title('Training Min Loss')
        return min_loss_dict

    def expend_labels_df(self, df):
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"
        temp_list = list(range(df.shape[1])) 
        temp_list = [x + df.shape[1] for x in temp_list]
        num_of_labels = df.shape[1]
        df = df.reindex(columns = [*df.columns.tolist(), *temp_list], fill_value = 0)
        assert df.shape[1] == 2 * num_of_labels, "Error in expend_labels_df function"
        return df
    
    def get_recieved_batches(self):
        """
        Returns a dictionary of recieved batches in the experiment phase.
        recived_batches_dict = {(source_name, worker_name): [batch_id,...]}
        """
        def recieved_batches_key(phase_name, source_name, worker_name):
            return f"phase:{phase_name},{source_name}->{worker_name}"

        phase_name = self.experiment_phase.get_name()
        recived_batches_dict = {}
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        for source_piece_inst in sources_pieces_list:
            source_name = source_piece_inst.get_source_name()
            target_workers_string = source_piece_inst.get_target_workers()
            target_workers_names = target_workers_string.split(',')
            for worker_db in workers_model_db_list:
                    worker_name = worker_db.get_worker_name()
                    if worker_name in target_workers_names:       # Check if the worker is in the target workers list of this source
                        for batch_id in range(source_piece_inst.get_num_of_batches()):
                            batch_db = worker_db.get_batch(source_name, str(batch_id))
                            if batch_db:    # if batch is recieved
                                recieved_batch_key_str = recieved_batches_key(phase_name, source_name, worker_name)
                                if recieved_batch_key_str not in recived_batches_dict:
                                    recived_batches_dict[recieved_batch_key_str] = []
                                recived_batches_dict[recieved_batch_key_str].append(batch_id)
        return recived_batches_dict

    def get_confusion_matrices(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False): 
        
        def build_worker_label_df(original_df, batch_ids, batch_size):
            rows_list = []

            for batch_id in batch_ids:
                # Calculate the start and end indices for the rows to be copied
                start_idx = batch_id * batch_size
                end_idx = (batch_id + 1) * batch_size
        
                # Extract the rows and append to the list
                batch_rows = original_df.iloc[start_idx:end_idx]
                rows_list.append(batch_rows)
    
                # Concatenate all the extracted rows into a new DataFrame
            df_worker_labels = pd.concat(rows_list, ignore_index=True)
            return df_worker_labels
        
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments" 
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"   
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        confusion_matrix_source_dict = {}
        confusion_matrix_worker_dict = {}
        recived_batches_dict = self.get_recieved_batches()
        for source_piece_inst in sources_pieces_list:
            nerltensorType = source_piece_inst.get_nerltensor_type()
            source_name = source_piece_inst.get_source_name()
            sourcePiece_csv_labels_path = source_piece_inst.get_pointer_to_sourcePiece_CsvDataSet_labels()
            df_actual_labels = pd.read_csv(sourcePiece_csv_labels_path)
            num_of_labels = df_actual_labels.shape[1]

            # build confusion matrix for each worker
            target_workers_string = source_piece_inst.get_target_workers()
            target_workers_names = target_workers_string.split(',')
            batch_size = source_piece_inst.get_batch_size()
            for worker_db in workers_model_db_list:
                worker_name = worker_db.get_worker_name()
                if worker_name not in target_workers_names:
                    continue
                worker_recived_batches_id = recived_batches_dict.get(f"phase:{self.experiment_phase.get_name()},{source_name}->{worker_name}")   # get a list of recived batches id for the worker
                df_worker_labels = build_worker_label_df(df_actual_labels, worker_recived_batches_id, batch_size)
                header_list = range(num_of_labels) 
                df_worker_labels.columns = header_list
                df_worker_labels = self.expend_labels_df(df_worker_labels)  #Now there is a csv file with the actual labels of the source piece and empty columns for the predict labels
                
                # Check if the actual labels are integers and the predict labels are floats, if so convert the actual labels to nerltensorType
                if nerltensorType == 'float':
                    if any(pd.api.types.is_integer_dtype(df_worker_labels[col]) for col in df_worker_labels.columns):
                        df_worker_labels = df_worker_labels.astype(float)
                        
                
                #build df_worker_labels with the actual labels and the predict labels
                index = 0
                for batch_id in worker_recived_batches_id:
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if not batch_db:             #It's not necessary to check if the batch is missing, because we already know wich batches are recieved
                        LOG_INFO(f"Batch {batch_id} is missing for worker {worker_name}")
                        continue
                    tensor_data = batch_db.get_tensor_data() 
                    tensor_data = tensor_data.reshape(batch_size, num_of_labels).copy()  # Make the tensor_data array writable
                    start_index = index * batch_size
                    end_index = (index + 1) * batch_size
                    df_worker_labels.iloc[start_index:end_index, num_of_labels:] = tensor_data
                    index += 1
                    
                if len(self.headers_list) == 1:   # One class
                    class_name = self.headers_list[0]
                    actual_labels = df_worker_labels.iloc[:, :num_of_labels].values.flatten().tolist()
                    # Predicted labels should be binary, threshold 0.5, turn every float to 1 if it's greater than 0.5, 0 otherwise
                    predict_labels = df_worker_labels.iloc[:, num_of_labels:].values.flatten().tolist()
                    predict_labels = [1.0 if label > 0.5 else 0.0 for label in predict_labels]
                    confusion_matrix = metrics.confusion_matrix(actual_labels, predict_labels)
                    confusion_matrix_source_dict[(source_name, worker_name, class_name)] = confusion_matrix
                    if (worker_name, class_name) not in confusion_matrix_worker_dict:
                        confusion_matrix_worker_dict[(worker_name, class_name)] = confusion_matrix
                    else:
                        confusion_matrix_worker_dict[(worker_name, class_name)] += confusion_matrix
                    
                else: # Multi-Class
                    #check if there is a sample with more than one predicted label
                    max_in_row = df_worker_labels.iloc[:, num_of_labels:].max(axis=1)
                    is_max = df_worker_labels.iloc[:, num_of_labels:].eq(max_in_row, axis=0)  #Get a DataFrame of boolean values where True indicates the maximum value in that row
                    max_counts = is_max.sum(axis=1)    #Get the number of maximum values in each row
                    has_multiple_max = max_counts.gt(1).any()  #boolean value: checks if there is at least one row with multiple maximum values in the predict labels

                    if has_multiple_max:
                        LOG_INFO(f"Worker {worker_name} has at least one sample with multiple predicted labels")
                        max_column_predict_index = is_max.apply(lambda row: list(row[row].index) if row.any() else [-1], axis=1).tolist()   # Generate a list of lists' each sublist has the index of the maximum value in the row
                        max_column_predict_index =[[int(predict_label) - num_of_labels for predict_label in prdict_indexes_sublist] for prdict_indexes_sublist in max_column_predict_index]  # fix the index to original labels index
                        max_column_labels_index = df_worker_labels.iloc[:, :num_of_labels].idxmax(axis=1).tolist()  # Get the index of the maximum actual value in each row

                    else:            # No sample with multiple predicted labels
                        # Take 2 lists from the df, one for the actual labels and one for the predict labels to build the confusion matrix
                        max_column_predict_index = df_worker_labels.iloc[:, num_of_labels:].idxmax(axis=1) 
                        max_column_predict_index = max_column_predict_index.tolist() 
                        max_column_predict_index = [int(predict_index) - num_of_labels for predict_index in max_column_predict_index] # fix the index to original labels index
                        max_column_labels_index = df_worker_labels.iloc[:, :num_of_labels].idxmax(axis=1)
                        max_column_labels_index = max_column_labels_index.tolist()
                    
                    # building confusion matrix for each class
                    for class_index, class_name in enumerate(self.headers_list):
                        if has_multiple_max:
                            class_predict_list = [1 if class_index in row_max_list else 0 for row_max_list in max_column_predict_index]
                        else:
                            class_predict_list = [1 if label_num == class_index else 0 for label_num in max_column_predict_index]   # 1 if the label is belong to the class, 0 otherwise
                        class_actual_list = [1 if label_num == class_index else 0 for label_num in max_column_labels_index]   # 1 if the label is belong to the class, 0 otherwise
                        labels = [0, 1]
                        confusion_matrix = metrics.confusion_matrix(class_actual_list, class_predict_list, labels=labels)  
                        confusion_matrix_source_dict[(source_name, worker_name, class_name)] = confusion_matrix
                        if (worker_name, class_name) not in confusion_matrix_worker_dict:
                            confusion_matrix_worker_dict[(worker_name, class_name)] = confusion_matrix
                        else:
                            confusion_matrix_worker_dict[(worker_name, class_name)] += confusion_matrix
        
        if plot:
            workers = sorted(list({tup[0] for tup in confusion_matrix_worker_dict.keys()}))
            classes = sorted(list({tup[1] for tup in confusion_matrix_worker_dict.keys()}))
            fig, ax = plt.subplots(nrows=len(workers), ncols=len(classes),figsize=(4*len(classes), 4*len(workers)), dpi=140)
            
            # Handle different combinations of workers and classes
            if len(workers) == 1 and len(classes) == 1:
                # Single worker, single class
                worker = workers[0]
                pred_class = classes[0]
                conf_mat = confusion_matrix_worker_dict[(worker, pred_class)]
                heatmap = sns.heatmap(data=conf_mat, ax=ax, annot=True, fmt="d", cmap='Blues', annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                cbar = heatmap.collections[0].colorbar
                cbar.ax.tick_params(labelsize=8)
                ax.set_title(f"{worker} , Class '{pred_class}'", fontsize=12)
                ax.tick_params(axis='both', which='major', labelsize=8)
                ax.set_xlabel("Predicted Label", fontsize=8)
                ax.set_ylabel("True Label", fontsize=8)
                ax.set_aspect('equal')
            elif len(workers) == 1 and len(classes) > 1:
                # Single worker, multiple classes
                worker = workers[0]
                for j, pred_class in enumerate(classes):
                    conf_mat = confusion_matrix_worker_dict[(worker, pred_class)]
                    heatmap = sns.heatmap(data=conf_mat, ax=ax[j], annot=True, fmt="d", cmap='Blues', annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                    cbar = heatmap.collections[0].colorbar
                    cbar.ax.tick_params(labelsize=8)
                    ax[j].set_title(f"{worker} , Class '{pred_class}'", fontsize=12)
                    ax[j].tick_params(axis='both', which='major', labelsize=8)
                    ax[j].set_xlabel("Predicted Label", fontsize=8)
                    ax[j].set_ylabel("True Label", fontsize=8)
                    ax[j].set_aspect('equal')
            elif len(workers) > 1 and len(classes) == 1:
                # Multiple workers, single class
                pred_class = classes[0]
                for i, worker in enumerate(workers):
                    conf_mat = confusion_matrix_worker_dict[(worker, pred_class)]
                    heatmap = sns.heatmap(data=conf_mat, ax=ax[i], annot=True, fmt="d", cmap='Blues', annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                    cbar = heatmap.collections[0].colorbar
                    cbar.ax.tick_params(labelsize=8)
                    ax[i].set_title(f"{worker} , Class '{pred_class}'", fontsize=12)
                    ax[i].tick_params(axis='both', which='major', labelsize=8)
                    ax[i].set_xlabel("Predicted Label", fontsize=8)
                    ax[i].set_ylabel("True Label", fontsize=8)
                    ax[i].set_aspect('equal')
            else:
                # Multiple workers, multiple classes
                for i, worker in enumerate(workers):
                    for j, pred_class in enumerate(classes):
                        conf_mat = confusion_matrix_worker_dict[(worker, pred_class)]
                        heatmap = sns.heatmap(data=conf_mat, ax=ax[i, j], annot=True, fmt="d", cmap='Blues', annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                        cbar = heatmap.collections[0].colorbar
                        cbar.ax.tick_params(labelsize=8)
                        ax[i, j].set_title(f"{worker} , Class '{pred_class}'", fontsize=12)
                        ax[i, j].tick_params(axis='both', which='major', labelsize=8)
                        ax[i, j].set_xlabel("Predicted Label", fontsize=8)
                        ax[i, j].set_ylabel("True Label", fontsize=8)
                        ax[i, j].set_aspect('equal')
            
            fig.subplots_adjust(wspace=0.4, hspace=0.4)
            plt.show()
            
        return confusion_matrix_source_dict, confusion_matrix_worker_dict
        
    def get_recieved_batches(self):
        """
        Returns a dictionary of recieved batches in the experiment phase.
        recived_batches_dict = {(source_name, worker_name): [batch_id,...]}
        """
        def recieved_batches_key(phase_name, source_name, worker_name):
            return f"phase:{phase_name},{source_name}->{worker_name}"

        phase_name = self.experiment_phase.get_name()
        recived_batches_dict = {}
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        for source_piece_inst in sources_pieces_list:
            source_name = source_piece_inst.get_source_name()
            if self.phase == PHASE_PREDICTION_STR:
                source_epoch = 1
            else:
                source_epoch = int(globe.components.sourceEpochs[source_name])
            target_workers_string = source_piece_inst.get_target_workers()
            target_workers_names = target_workers_string.split(',')
            for worker_db in workers_model_db_list:
                    worker_name = worker_db.get_worker_name()
                    if worker_name in target_workers_names:       # Check if the worker is in the target workers list of this source
                        for batch_id in range(source_epoch * source_piece_inst.get_num_of_batches()):
                            batch_db = worker_db.get_batch(source_name, str(batch_id))
                            if batch_db:    # if batch is recieved
                                recieved_batch_key_str = recieved_batches_key(phase_name, source_name, worker_name)
                                if recieved_batch_key_str not in recived_batches_dict:
                                    recived_batches_dict[recieved_batch_key_str] = []
                                recived_batches_dict[recieved_batch_key_str].append(batch_id)
        return recived_batches_dict
    
    def get_missed_batches(self):
        """
        Returns a dictionary of missed batches in the experiment phase.
        {(source_name, worker_name): [batch_id,...]}
        """
        def missed_batches_key(phase_name, source_name, worker_name):
            return f"phase:{phase_name},{source_name}->{worker_name}"

        phase_name = self.experiment_phase.get_name()
        missed_batches_dict = {}
        sources_pieces_list = self.experiment_phase.get_sources_pieces()
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        for source_piece_inst in sources_pieces_list:
            source_name = source_piece_inst.get_source_name()
            source_policy = globe.components.sources_policy_dict[source_name]   # 0 -> casting , 1 -> round robin, 2 -> random
            if self.phase == PHASE_PREDICTION_STR:
                source_epoch = 1
            else:
                source_epoch = int(globe.components.sourceEpochs[source_name])
            target_workers_string = source_piece_inst.get_target_workers()
            target_workers_names = target_workers_string.split(',')
            if source_policy == '0':  # casting policy
                for worker_db in workers_model_db_list:
                    worker_name = worker_db.get_worker_name()
                    if worker_name in target_workers_names:       # Check if the worker is in the target workers list of this source
                        for batch_id in range(source_epoch * source_piece_inst.get_num_of_batches()):
                            batch_db = worker_db.get_batch(source_name, str(batch_id))
                            if not batch_db:  # if batch is missing
                                missed_batch_key_str = missed_batches_key(phase_name, source_name, worker_name)
                                if  missed_batch_key_str not in missed_batches_dict:
                                    missed_batches_dict[missed_batch_key_str] = []
                                missed_batches_dict[missed_batch_key_str].append(batch_id)
            elif source_policy == '1':  # round robin policy
                number_of_workers = len(target_workers_names)
                batches_indexes = [i for i in range(source_epoch * source_piece_inst.get_num_of_batches())]
                batch_worker_tuple = [(batch_index, target_workers_names[batch_index % number_of_workers]) for batch_index in batches_indexes]  # (batch_index, worker_name_that_should_recive_the_batch)
                worker_batches_dict = {worker_name: [] for worker_name in target_workers_names}   # Create a dictionary to hold batches id for each worker
                for batch_index, worker_name in batch_worker_tuple:
                    worker_batches_dict[worker_name].append(batch_index)            
                for worker_db in workers_model_db_list:
                    worker_name = worker_db.get_worker_name()
                    if worker_name in target_workers_names:       # Check if the worker is in the target workers list of this source
                        for batch_id in worker_batches_dict[worker_name]:
                            batch_db = worker_db.get_batch(source_name, str(batch_id))
                            if not batch_db:
                                missed_batch_key_str = missed_batches_key(phase_name, source_name, worker_name)
                                if  missed_batch_key_str not in missed_batches_dict:
                                    missed_batches_dict[missed_batch_key_str] = []
                                missed_batches_dict[missed_batch_key_str].append(batch_id)
            elif source_policy == '2':  # random policy
                LOG_INFO(f"Source {source_name} policy is random, it's not posiblle check for missed batches")
                break
        return missed_batches_dict
    
    def plot_batches_status(self, plot=False):
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        workers_names = [worker_model_db.get_worker_name() for worker_model_db in workers_model_db_list]
        received_batches = self.get_recieved_batches()
        missed_batches = self.get_missed_batches()

        # Initialize dictionaries to store batch counts for each worker
        batches_received = {worker: 0 for worker in workers_names}
        batches_dropped = {worker: 0 for worker in workers_names}
            
        # Fill the dictionaries with the counts of received and missed batches
        for key, batches in received_batches.items():
            worker = key.split('->')[-1]
            batches_received[worker] += len(batches)
            
        for key, batches in missed_batches.items():
            worker = key.split('->')[-1]
            batches_dropped[worker] += len(batches)
            
        # Create a DataFrame for plotting
        workers_comm_dict = {
            'Worker': list(batches_received.keys()),
            'batches_received': list(batches_received.values()),
            'batches_dropped': list(batches_dropped.values())
        }
        df = pd.DataFrame(workers_comm_dict)
            
        # Sort the DataFrame by the worker names
        df = df.sort_values(by='Worker')
        
        # Plotting
        if plot:
            plt.figure(figsize=(10, 6))
            data = pd.melt(df, id_vars=['Worker'], value_vars=['batches_received', 'batches_dropped'])
            batches_stats = sns.barplot(x='Worker', y='value', hue='variable', data=data, order=sorted(workers_names))
            plt.ylabel('Number Of Batches')
            plt.xlabel('Worker')
            plt.title(f"Received & Dropped Batches in Phase: ({self.experiment_phase.get_name()})")

            batches_stats.legend(loc='upper right', bbox_to_anchor=(1.5, 0.2), shadow=True, ncol=1)
            plt.show()       
    
    def get_communication_stats_workers(self):
        # return dictionary of {worker : {communication_stats}}
        communication_stats_workers_dict = OrderedDict()
        workers_dict = self.nerl_comm_db.get_workers()
        for worker_name in workers_dict:
            communication_stats_workers_dict[worker_name] = workers_dict[worker_name].get_as_dict()
        return communication_stats_workers_dict
    

    def get_communication_stats_sources(self):
        # return dictionary of {source : {communication_stats}}
        communication_stats_sources_dict = OrderedDict()
        sources_dict = self.nerl_comm_db.get_sources()
        for source_name in sources_dict:
            communication_stats_sources_dict[source_name] = sources_dict[source_name].get_as_dict()
        return communication_stats_sources_dict
    
    def get_communication_stats_clients(self):
        # return dictionary of {client : {communication_stats}}
        communication_stats_clients_dict = OrderedDict()
        clients_dict = self.nerl_comm_db.get_clients()
        for client_name in clients_dict:
            communication_stats_clients_dict[client_name] = clients_dict[client_name].get_as_dict()
        return communication_stats_clients_dict

    def get_communication_stats_routers(self):
        # return dictionary of {router : {communication_stats}}
        communication_stats_routers_dict = OrderedDict()
        routers_dict = self.nerl_comm_db.get_routers()
        for router_name in routers_dict:
            communication_stats_routers_dict[router_name] = routers_dict[router_name].get_as_dict()
        return communication_stats_routers_dict

    def get_communication_stats_main_server(self):
        # return dictionary of {main_server : {communication_stats}}
        main_server_communication_stats = self.nerl_comm_db.get_main_server().get_as_dict()
        return main_server_communication_stats
    
    def get_performance_stats_clients(self):
        # return dictionary of {client : {performance_stats}}
        performance_stats_clients_dict = OrderedDict()
        clients_dict = self.nerl_perf_db.get_clients()
        for client_name in clients_dict:
            performance_stats_clients_dict[client_name] = clients_dict[client_name].get_as_dict()
        return performance_stats_clients_dict

    def get_actual_frequencies_of_sources(self):
        # return dictionary of {source : {actual_freq}}
        actual_frequencies_of_sources_dict = OrderedDict()
        sources_dict = self.nerl_comm_db.get_sources()
        for source_name, source_db_dict in sources_dict.items():
            actual_frequencies_of_sources_dict[source_name] = source_db_dict.get_as_dict()["actual_frequency"]
        return actual_frequencies_of_sources_dict

    def get_model_performence_stats(self , confusion_matrix_worker_dict , show : bool = False , saveToFile : bool = False, printStats = False) -> dict:
        """
        Returns a dictionary of {(worker, class): {Performence_Stat : VALUE}}} for each worker and class in the experiment.
        Performence Statistics Available are: TN, FP, FN, TP, Accuracy, Balanced Accuracy, Precision, Recall, True Negative Rate, Informedness, F1
        """
        assert self.experiment_flow_type == "classification", "This function is only available for classification experiments"
        workers_performence = OrderedDict()
        for (worker_name, class_name) in confusion_matrix_worker_dict.keys():
            workers_performence[(worker_name, class_name)] = OrderedDict()
            matrix = confusion_matrix_worker_dict[(worker_name, class_name)]
            if len(matrix.shape) == 1 or matrix.shape == (1, 1):  # if the matrix is 1D or 1x1 skip to prevent errors in the calculations
                continue
            tn, fp, fn, tp = matrix.ravel()
            if printStats:
                LOG_INFO(f"worker {worker_name} class: {class_name} tn: {tn}, fp: {fp}, fn: {fn}, tp: {tp}")
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

            workers_performence[(worker_name, class_name)]['TN'] = tn
            workers_performence[(worker_name, class_name)]['FP'] = fp
            workers_performence[(worker_name, class_name)]['FN'] = fn
            workers_performence[(worker_name, class_name)]['TP'] = tp
            workers_performence[(worker_name, class_name)]['Accuracy'] = acc
            workers_performence[(worker_name, class_name)]['Balanced Accuracy'] = bacc
            workers_performence[(worker_name, class_name)]['Precision'] = ppv
            workers_performence[(worker_name, class_name)]['Recall'] = tpr
            workers_performence[(worker_name, class_name)]['True Negative Rate'] = tnr
            workers_performence[(worker_name, class_name)]['Informedness'] = inf
            workers_performence[(worker_name, class_name)]['F1'] = f1
            
        df = pd.DataFrame.from_dict(workers_performence, orient='index')
        stats = list(df.columns)
        df.reset_index(inplace=True)
        df.columns = ['Worker', 'Class'] + stats
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

    def get_predict_regression_stats(self , plot : bool = False , saveToFile : bool = False):
        pass
    
    def get_total_bytes(self):
        # Return the total bytes sent and received in the experiment
        comm_stats_main_server = self.get_communication_stats_main_server()
        comm_stats_router = self.get_communication_stats_routers()
        comm_stats_clients = self.get_communication_stats_clients()
        comm_stats_sources = self.get_communication_stats_sources()
        bytes = 0
        for client in comm_stats_clients:
            bytes += comm_stats_clients[client]['bytes_sent']
            bytes += comm_stats_clients[client]['bytes_received']
        for source in comm_stats_sources:
            bytes += comm_stats_sources[source]['bytes_sent']
            bytes += comm_stats_sources[source]['bytes_received']
        for router in comm_stats_router:
            bytes += comm_stats_router[router]['bytes_sent']
            bytes += comm_stats_router[router]['bytes_received']
        bytes += comm_stats_main_server['bytes_sent']
        bytes += comm_stats_main_server['bytes_received']
        return bytes
        


