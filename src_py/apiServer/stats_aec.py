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

from stats import Stats

class StatsAEC():
    
    def __init__(self, stats: Stats):
        self.stats = stats
        self.csv_path = stats.experiment_phase.get_sources_pieces()[0].get_csv_dataset_parent().get_csv_path()
        self.num_of_overall_samples = stats.experiment_phase.get_sources_pieces()[0].get_csv_dataset_parent().get_total_num_of_samples()
        self.loss_values_pairs = {}
        self.source_pieces = stats.experiment_phase.get_sources_pieces()
        
        
    def get_aec_loss(self, plot=False):
        loss_dict = {}
        workers_model_db_list = self.stats.nerl_model_db.get_workers_model_db_list()
        total_batches_list = [worker.get_total_batches() for worker in workers_model_db_list]
        max_batches = max(total_batches_list)
        
        for worker_db in workers_model_db_list:
            worker_name = worker_db.get_worker_name()
            batches_ts_tensor_data_dict = worker_db.get_batches_ts_tensor_data_dict()
            sorted_batches_ts_tensor_data_dict = dict(sorted(batches_ts_tensor_data_dict.items()))
            loss_dict[worker_name] = [sorted_batches_ts_tensor_data_dict[key][0] for key in sorted(sorted_batches_ts_tensor_data_dict)]
            batches_ts_tensor_data_dict = worker_db.get_batches_ts_tensor_data_dict()
            sorted_batches_ts_tensor_data_dict = dict(sorted(batches_ts_tensor_data_dict.items()))
            loss_dict[worker_name] = [sorted_batches_ts_tensor_data_dict[key][0] for key in sorted(sorted_batches_ts_tensor_data_dict)]
            
        for worker_name in loss_dict:
            loss_dict[worker_name] = [float(arr) for sublist in loss_dict[worker_name] for arr in sublist]
            loss_dict[worker_name] += [None] * (max_batches - len(loss_dict[worker_name]))            

        df = pd.DataFrame(loss_dict)
        self.loss_ts_pd = df
        
        if plot:
            sns.lineplot(data=df)
            plt.xlabel('Batch Num.')
            plt.ylabel('Loss Value')
            plt.title('Training Loss Function')
        return df

        
    def get_aec_boundaries(self, plot=False):
        upper_boundaries_dict = {}
        lower_boundaries_dict = {}
        workers_model_db_list = self.stats.nerl_model_db.get_workers_model_db_list()
        total_batches_list = [worker.get_total_batches() for worker in workers_model_db_list]
        max_batches = max(total_batches_list)
        
        for worker_db in workers_model_db_list:
            worker_name = worker_db.get_worker_name()
            batches_ts_tensor_data_dict = worker_db.get_batches_batchid_tensor_data_dict()
            sorted_keys = sorted(batches_ts_tensor_data_dict.keys())
            upper_boundaries_dict[worker_name] = [batches_ts_tensor_data_dict[key][1] for key in sorted_keys]
            lower_boundaries_dict[worker_name] = [batches_ts_tensor_data_dict[key][2] for key in sorted_keys]
            
        for worker_name in upper_boundaries_dict:
            upper_boundaries_dict[worker_name] = [float(arr) for sublist in upper_boundaries_dict[worker_name] for arr in sublist]
            lower_boundaries_dict[worker_name] = [float(arr) for sublist in lower_boundaries_dict[worker_name] for arr in sublist]
            upper_boundaries_dict[worker_name] += [None] * (max_batches - len(upper_boundaries_dict[worker_name]))
            lower_boundaries_dict[worker_name] += [None] * (max_batches - len(lower_boundaries_dict[worker_name]))

        df_upper = pd.DataFrame(upper_boundaries_dict).sort_index(axis=1)
        df_lower = pd.DataFrame(lower_boundaries_dict).sort_index(axis=1)
        
        # Take 10% of the data for better visualization
        if len(df_upper) > 100:
            df_upper = df_upper.iloc[::len(df_upper) // 100, :]
            df_lower = df_lower.iloc[::len(df_lower) // 100, :]
        else:
            df_upper = df_upper.iloc[::len(df_upper), :]
            df_lower = df_lower.iloc[::len(df_lower), :]
        
        if plot:
            for worker_name in df_upper:
                # Calculate the seperator to be the average of the upper and lower boundaries
                seperator = (df_upper[worker_name] + df_lower[worker_name]) / 2
                plt.figure(figsize=(12, 8))
                plt.plot(df_upper[worker_name], label='Upper Boundary', color='C0')
                plt.fill_between(df_upper[worker_name].index, df_upper[worker_name] - df_upper[worker_name].std(), df_upper[worker_name] + df_upper[worker_name].std(), color='C0', alpha=0.2)
                plt.plot(df_lower[worker_name], label='Lower Boundary', color='C1')
                plt.fill_between(df_lower[worker_name].index, df_lower[worker_name] - df_lower[worker_name].std(), df_lower[worker_name] + df_lower[worker_name].std(), color='C1', alpha=0.2)
                plt.plot(seperator, label='Seperator', color='C2')
                plt.fill_between(seperator.index, seperator + seperator.std(), seperator - seperator.std(), color='C2', alpha=0.2)
                # plt.xscale('log') # For better visualization
                plt.xlabel('Batch Num.')
                plt.ylabel('Boundary Value')
                plt.title(f'Training Boundaries {worker_name}')
                plt.legend()
                plt.show()
        return df_upper, df_lower
        
        
    def get_labels(self):
        source_piece_labels_dict = {}
        for source_piece in self.source_pieces:
            number_of_samples = source_piece.get_num_of_batches() * source_piece.get_batch_size()
            skip_rows = source_piece.get_starting_offset()
            df = pd.read_csv(self.csv_path, skiprows = skip_rows, nrows = number_of_samples, header = None)
            labels_df = df.iloc[:, int(source_piece.get_num_of_features()) : int(source_piece.get_num_of_features()) + int(source_piece.get_num_of_labels())]
            source_piece_labels_dict[source_piece.get_source_name()] = labels_df
        # set volimn name to 'Label' for better visualization
        for source_name in source_piece_labels_dict.keys():
            source_piece_labels_dict[source_name].columns = ['Label']
        return source_piece_labels_dict
            
    def plot_errors(self):
        labels_by_source_piece = self.get_labels()
        loss_values_dict = self.get_loss_batchid_per_worker_by_source()
        batchesIDs_per_worker = {}
        for worker in loss_values_dict.keys():
            self.loss_values_pairs[worker] = {}
            for source_name in loss_values_dict[worker].keys():
                source_piece_labels_df = labels_by_source_piece[source_name]
                loss_batchid_list = []
                for batchID, loss_tensor in loss_values_dict[worker][source_name]:
                    for i in range(self.batch_size):
                        loss_batchid_list.append((batchID, (i + int(batchID) * self.batch_size) % source_piece_labels_df.shape[0], float(loss_tensor[i]))) # Modulo for epochs_num > 1
                loss_labels_pairs = []
                batchesIDs = np.unique([batchID for batchID, _, _ in loss_batchid_list])
                batchesIDs_per_worker[worker] = batchesIDs
                print(f'Worker {worker} batches IDs list: {batchesIDs}')
                for batchID, sampleID, loss_val in loss_batchid_list:
                    loss_labels_pairs.append((loss_val, float(source_piece_labels_df['Label'].iloc[sampleID]))) # Modulo to handle the case of more than one epoch
                self.loss_values_pairs[worker][source_name] = loss_labels_pairs
            total_loss_labels_pairs = []
            for source_name in self.loss_values_pairs[worker].keys():
                total_loss_labels_pairs.extend(self.loss_values_pairs[worker][source_name])
            anomalies_loss = [loss for loss, label in total_loss_labels_pairs if label == 1.0]
            normal_loss = [loss for loss, label in total_loss_labels_pairs if label == 0.0]
            _, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8)) # ! Current anomaly detection AEC has only 2 classes (anomaly/normal)
            min_loss = min(min(anomalies_loss), min(normal_loss))
            max_loss = max(np.median(anomalies_loss), np.median(normal_loss))
            # if max_loss > 1: # To handle the case of the loss being large at the beginning of the training (for better visualization)
            #     max_loss -= 1
            bins = np.linspace(min_loss, max_loss, 100)

            ax1.hist(anomalies_loss, bins=bins, color='red', alpha=0.7, label='Anomalies')
            ax1.set_title(f'{worker} Anomalies Loss Values')
            ax1.set_xlabel('Loss')
            ax1.set_ylabel('Count')
            ax1.set_xlim([min_loss, max_loss])
            ax1.legend()

            ax2.hist(normal_loss, bins=bins, color='blue', alpha=0.7, label='Normal')
            ax2.set_title(f'{worker} Normal Loss Values')
            ax2.set_xlabel('Loss')
            ax2.set_ylabel('Count')
            ax2.set_xlim([min_loss, max_loss])
            ax2.legend()
            
            plt.tight_layout()
            plt.show()
        # Check weather the workers got the same batchesIDs
        # for worker in batchesIDs_per_worker.keys():
        #     for other_worker in batchesIDs_per_worker.keys():
        #         if worker != other_worker:
        #             print(f'Workers {worker} and {other_worker} have the same batches IDs: {np.intersect1d(batchesIDs_per_worker[worker], batchesIDs_per_worker[other_worker])}')


    def plot_average_aec_errors(self):
        plt.figure(figsize=(12, 8))
        for worker in self.loss_values_pairs.keys():
            anomaly_loss_values = []
            normal_loss_values = []
            for source_name in self.loss_values_pairs[worker]:
                for loss, label in self.loss_values_pairs[worker][source_name]:
                    if label == 1.0:
                        anomaly_loss_values.append(loss)
                    elif label == 0.0:
                        normal_loss_values.append(loss)
                    else:
                        print("Unknown Label Value (Should be 1.0/0.0")
            avg_anomaly_loss = np.mean(anomaly_loss_values)
            avg_normal_loss = np.mean(normal_loss_values)
            plt.bar(f'{worker} Anomaly', avg_anomaly_loss, color='tab:blue')
            plt.bar(f'{worker} Normal', avg_normal_loss, color='tab:orange')

        plt.title("Average Errors Per Worker")
        plt.ylabel("Average Loss")
        plt.tight_layout()
        plt.show()


    def get_loss_batchid_per_worker_by_source(self) -> dict:
        workers_model_db_list = self.stats.nerl_model_db.get_workers_model_db_list()
        loss_values_dict = {}
        for worker_db in workers_model_db_list:
            worker_name = worker_db.get_worker_name()
            batches_batchid_tensor_data_dict = worker_db.get_batches_batchid_tensor_data_dict()
            worker_source_loss_dict = {}
            for source_name, batchID in batches_batchid_tensor_data_dict.keys():
                if source_name not in worker_source_loss_dict:
                    worker_source_loss_dict[source_name] = []
                worker_source_loss_dict[source_name].append((batchID, batches_batchid_tensor_data_dict[(source_name, batchID)][3:]))
            loss_values_dict[worker_name] = worker_source_loss_dict
        self.batch_size = len(batches_batchid_tensor_data_dict[(source_name, batchID)][3:])
        return loss_values_dict
    
    def get_false_alarm_rate(self, conf_mats_workers):
        false_alarm_rate_dict = {}
        for worker_name, _ in conf_mats_workers.items():
            conf_mat = conf_mats_workers[worker_name]
            false_alarm_rate_dict[worker_name] = conf_mat[0][1] / (conf_mat[0][1] + conf_mat[0][0])
        print(f'Average False Alarm Rate: {np.mean(list(false_alarm_rate_dict.values()))*100:.4f}%')
        return false_alarm_rate_dict
    
    def get_detection_rate(self, conf_mats_workers):
        detection_rate_dict = {}
        for worker_name, _ in conf_mats_workers.items():
            conf_mat = conf_mats_workers[worker_name]
            detection_rate_dict[worker_name] = conf_mat[1][1] / (conf_mat[1][1] + conf_mat[1][0])
        print(f'Average Detection Rate: {np.mean(list(detection_rate_dict.values()))*100:.4f}%')
        print(f'Average Missing Alarm Rate: {100 - np.mean(list(detection_rate_dict.values()))*100:.4f}%') # Optional Print
        return detection_rate_dict
