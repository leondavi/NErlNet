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
            batches_ts_tensor_data_dict = worker_db.get_batches_ts_tensor_data_dict()
            sorted_batches_ts_tensor_data_dict = dict(sorted(batches_ts_tensor_data_dict.items()))
            upper_boundaries_dict[worker_name] = [sorted_batches_ts_tensor_data_dict[key][1] for key in sorted(sorted_batches_ts_tensor_data_dict)]
            lower_boundaries_dict[worker_name] = [sorted_batches_ts_tensor_data_dict[key][2] for key in sorted(sorted_batches_ts_tensor_data_dict)]
            
        for worker_name in upper_boundaries_dict:
            upper_boundaries_dict[worker_name] = [float(arr) for sublist in upper_boundaries_dict[worker_name] for arr in sublist]
            upper_boundaries_dict[worker_name] += [None] * (max_batches - len(upper_boundaries_dict[worker_name]))
        for worker_name in lower_boundaries_dict:
            lower_boundaries_dict[worker_name] = [float(arr) for sublist in lower_boundaries_dict[worker_name] for arr in sublist]
            lower_boundaries_dict[worker_name] += [None] * (max_batches - len(lower_boundaries_dict[worker_name]))

        df_upper = pd.DataFrame(upper_boundaries_dict).sort_index(axis=1)
        df_lower = pd.DataFrame(lower_boundaries_dict).sort_index(axis=1)
        
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
                plt.xlabel('Batch Num.')
                plt.ylabel('Boundary Value')
                plt.title(f'Training Boundaries {worker_name}')
                plt.legend()
                plt.show()
        return df_upper, df_lower
