from collections import OrderedDict
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
        self.phase = self.experiment_phase.get_phase_type()
        self.name = self.experiment_phase.get_name()
        self.loss_ts_pd = None
        self.missed_batches_warning_msg = False
        self.experiment_flow_type = self.experiment_phase.get_experiment_flow_type()
        if (self.phase == PHASE_PREDICTION_STR):
            for source_piece_inst in self.experiment_phase.get_sources_pieces():
                csv_dataset = source_piece_inst.get_csv_dataset_parent()
                source_piece_csv_labels_file = csv_dataset.genrate_source_piece_ds_csv_file_labels(source_piece_inst, self.phase)
                source_piece_inst.set_pointer_to_sourcePiece_CsvDataSet_labels(source_piece_csv_labels_file)
            self.headers_list = csv_dataset.get_headers_row()

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
    def get_loss_by_source(self , plot : bool = False , saveToFile : bool = False): # Todo change i
        """
        Returns a dictionary of {source : DataFrame[BatchID,'w1','w2'...'wn']} for each source in the experiment.
        """
        pass

    def get_loss_ts(self , plot : bool = False , saveToFile : bool = False, log : bool = False): # Todo change it
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
            batches_ts_tansor_data_dict =worker_db.get_batches_ts_tansor_data_dict()
            sorted_batches_ts_tansor_data_dict = dict(sorted(batches_ts_tansor_data_dict.items()))
            loss_dict[worker_name] = [sorted_batches_ts_tansor_data_dict[key] for key in sorted(sorted_batches_ts_tansor_data_dict)]
            
        # Convert NumPy arrays to floats
        for worker_name in loss_dict:
            loss_dict[worker_name] = [float(arr) for sublist in loss_dict[worker_name] for arr in sublist]
            loss_dict[worker_name] += [None] * (max_batches - len(loss_dict[worker_name]))

        df = pd.DataFrame(loss_dict)
        self.loss_ts_pd = df
        #print(df)
        
        if plot:
            sns.lineplot(data=df)
            plt.xlabel('Batch Num.')
            plt.ylabel('Loss Value')
            if log:
                plt.yscale('log')
                plt.xscale('log')
            plt.title('Training Loss Function')
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


        # if plot: 
        #     plt.figure(figsize = (30,15), dpi = 150)
        #     plt.rcParams.update({'font.size': 22})
        #     for worker_name, loss_list in loss_dict.items():
        #         plt.plot(loss_list, label=worker_name)
        #     plt.legend(list(loss_dict.keys()))
        #     plt.xlabel('Batch Number' , fontsize=30)
        #     plt.ylabel('Loss' , fontsize=30) 
        #     plt.yscale('log')
        #     plt.xlim(left=0)
        #     plt.ylim(bottom=0)
        #     plt.title('Training Loss Function')
        #     plt.grid(visible=True, which='major', linestyle='-')
        #     plt.minorticks_on()
        #     plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)
        #     plt.show()
        #     plt.savefig(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Training/Loss_graph.png')
         
    def expend_labels_df(self, df):
        assert self.phase == PHASE_PREDICTION_STR, "This function is only available for predict phase"
        temp_list = list(range(df.shape[1])) 
        temp_list = [x + df.shape[1] for x in temp_list]
        num_of_labels = df.shape[1]
        df = df.reindex(columns = [*df.columns.tolist(), *temp_list], fill_value = 0)
        assert df.shape[1] == 2 * num_of_labels, "Error in expend_labels_df function"
        return df

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
            num_of_labels = df_actual_labels.shape[1]
            header_list = range(num_of_labels) 
            df_actual_labels.columns = header_list
            df_actual_labels = self.expend_labels_df(df_actual_labels)
            #print(df_actual_labels)
            source_name = source_piece_inst.get_source_name()

            # build confusion matrix for each worker
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
                        if not self.missed_batches_warning_msg:
                            LOG_WARNING(f"missed batches")
                            self.missed_batches_warning_msg = True
                        starting_offset = source_piece_inst.get_starting_offset()
                        df_worker_labels.iloc[batch_id * batch_size: (batch_id + 1) * batch_size, num_of_labels:] = None # set the actual label to None for the predict labels in the df 
                        worker_missed_batches[(worker_name, source_name, str(batch_id))] = (starting_offset + batch_id * batch_size, batch_size)  # save the missing batch
                
                df_worker_labels = df_worker_labels.dropna()
                # print(df_worker_labels)
                for batch_id in range(total_batches_per_source):
                    batch_db = worker_db.get_batch(source_name, str(batch_id))
                    if batch_db:
                        # counter = according indexs of array 
                        # cycle = according indexs of panadas (with jump)
                        cycle = int(batch_db.get_batch_id())
                        tensor_data = batch_db.get_tensor_data()
                        # print(f"tensor_data shape: {tensor_data.shape}")
                        tensor_data = tensor_data.reshape(batch_size, num_of_labels) 
                        #print(df_worker_labels)
                        #print(tensor_data)
                        start_index = cycle * batch_size
                        end_index = (cycle + 1) * batch_size
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = None # Fix an issue of pandas of incompatible dtype
                        df_worker_labels.iloc[start_index:end_index, num_of_labels:] = tensor_data
                        # print(df_worker_labels)

                if len(self.headers_list) == 1:
                    class_name = self.headers_list[0]
                    actual_labels = df_worker_labels.iloc[:, :num_of_labels].values.flatten().tolist()
                    predict_labels = df_worker_labels.iloc[:, num_of_labels:].values.flatten().tolist()
                    confusion_matrix = metrics.confusion_matrix(actual_labels, predict_labels)
                    confusion_matrix_source_dict[(source_name, worker_name, class_name)] = confusion_matrix
                    if (worker_name, class_name) not in confusion_matrix_worker_dict:
                        confusion_matrix_worker_dict[(worker_name, class_name)] = confusion_matrix
                    else:
                        confusion_matrix_worker_dict[(worker_name, class_name)] += confusion_matrix
                    
                else: # Multi-Class
                    # Take 2 list from the df, one for the actual labels and one for the predict labels to build the confusion matrix
                    max_column_predict_index = df_worker_labels.iloc[:, num_of_labels:].idxmax(axis=1) 
                    max_column_predict_index = max_column_predict_index.tolist() 
                    max_column_predict_index = [int(predict_index) - num_of_labels for predict_index in max_column_predict_index] # fix the index to original labels index
                    max_column_labels_index = df_worker_labels.iloc[:, :num_of_labels].idxmax(axis=1)
                    max_column_labels_index = max_column_labels_index.tolist()
                    
                    # building confusion matrix for each class
                    for class_index, class_name in enumerate(self.headers_list):
                        class_actual_list = [1 if label_num == class_index else 0 for label_num in max_column_labels_index]   # 1 if the label is belong to the class, 0 otherwise
                        class_predict_list = [1 if label_num == class_index else 0 for label_num in max_column_predict_index]   # 1 if the label is belong to the class, 0 otherwise
                        confusion_matrix = metrics.confusion_matrix(class_actual_list, class_predict_list)  
                        #confusion_matrix_np = confusion_matrix.to_numpy()
                        confusion_matrix_source_dict[(source_name, worker_name, class_name)] = confusion_matrix
                        if (worker_name, class_name) not in confusion_matrix_worker_dict:
                            confusion_matrix_worker_dict[(worker_name, class_name)] = confusion_matrix
                        else:
                            confusion_matrix_worker_dict[(worker_name, class_name)] += confusion_matrix
                

        if plot:
            workers = sorted(list({tup[0] for tup in confusion_matrix_worker_dict.keys()}))
            classes = sorted(list({tup[1] for tup in confusion_matrix_worker_dict.keys()}))
            fig, ax = plt.subplots(nrows=len(workers), ncols=len(classes),figsize=(4*len(classes),4*len(workers)),dpi=140)
            if len(classes) > 1:
                for i , worker in enumerate(workers): 
                    for j , pred_class in enumerate(classes):
                        conf_mat = confusion_matrix_worker_dict[(worker , pred_class)]
                        # print(f"conf_mat: {conf_mat}")
                        heatmap = sns.heatmap(data=conf_mat ,ax=ax[i,j], annot=True , fmt="d", cmap='Blues',annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                        cbar = heatmap.collections[0].colorbar
                        cbar.ax.tick_params(labelsize = 8)
                        ax[i, j].set_title(f"{worker} , Class '{pred_class}'" , fontsize=12)
                        ax[i, j].tick_params(axis='both', which='major', labelsize=8) 
                        ax[i, j].set_xlabel("Predicted Label" , fontsize=8)
                        ax[i, j].set_ylabel("True Label" , fontsize=8)
                        ax[i, j].set_aspect('equal')
            else:
                for i, worker in enumerate(workers):
                    conf_mat = confusion_matrix_worker_dict[(worker , classes[0])]
                    heatmap = sns.heatmap(data=conf_mat ,ax=ax[i], annot=True , fmt="d", cmap='Blues',annot_kws={"size": 8}, cbar_kws={'pad': 0.1})
                    cbar = heatmap.collections[0].colorbar
                    cbar.ax.tick_params(labelsize = 8)
                    ax[i].set_title(f"{worker} , Class '{classes[0]}'" , fontsize=12)
                    ax[i].tick_params(axis='both', which='major', labelsize=8) 
                    ax[i].set_xlabel("Predicted Label" , fontsize=8)
                    ax[i].set_ylabel("True Label" , fontsize=8)
                    ax[i].set_aspect('equal')
            fig.subplots_adjust(wspace=0.4 , hspace=0.4)
            plt.show()
                
        

        return confusion_matrix_source_dict, confusion_matrix_worker_dict
    
    def get_missed_batches(self):
        """
        Returns a list of missed batches in the experiment phase.
        {(source_name, worker_name): [batch_id,...]}
        """
        def missed_batches_key(phase_name, source_name, worker_name):
            return f"phase:{phase_name},{source_name}->{worker_name}"
        
        if self.phase == PHASE_PREDICTION_STR:
            phase_name = self.experiment_phase.get_name()
            missed_batches_dict = {}
            sources_pieces_list = self.experiment_phase.get_sources_pieces()
            workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
            for source_piece_inst in sources_pieces_list:
                source_name = source_piece_inst.get_source_name()
                for worker_db in workers_model_db_list:
                    worker_name = worker_db.get_worker_name()
                    total_batches_per_source = worker_db.get_total_batches_per_source(source_name)
                    for batch_id in range(total_batches_per_source):
                        batch_db = worker_db.get_batch(source_name, str(batch_id))
                        if not batch_db:  # if batch is missing
                            missed_batch_key_str = missed_batches_key(phase_name, source_name, worker_name)
                            if  missed_batch_key_str not in missed_batches_dict:
                                missed_batches_dict[missed_batch_key_str] = []
                            missed_batches_dict[missed_batch_key_str].append(batch_id)
        #print(f"missed_batches_dict: {missed_batches_dict}")
        return missed_batches_dict

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
            tn, fp, fn, tp = confusion_matrix_worker_dict[(worker_name, class_name)].ravel()
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


