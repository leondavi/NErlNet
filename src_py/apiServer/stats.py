from collections import OrderedDict
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay, accuracy_score
import matplotlib.pyplot as plt
from datetime import datetime
from pathlib import Path
from experiment_phase import *
import globalVars as globe
from definitions import *
import pandas as pd
import numpy as np

MATRIX_DISP_SCALING = 5
class Stats():

    def __init__(self, experiment_phase: ExperimentPhase):
        self.experiment_phase = experiment_phase
        self.nerl_model_db = self.experiment_phase.get_nerl_model_db()
        self.phase = self.experiment_phase.get_phase_type()
        self.name = self.experiment_phase.get_name()

        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment_phase.get_experiment_flow_name()}').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment_phase.get_phase_type()}/{self.experiment_phase.get_experiment_flow_name()}').mkdir(parents=True, exist_ok=True)
        self.exp_path = f'{self.experiment_phase.get_experiment_flow_name()}_{datetime.now().strftime("%Y_%m_%d_%H_%M_%S")}'

    def get_phase(self):
        return self.phase
    
    def get_name(self):
        return self.name
    
       # TODO 
    def get_loss_foh_missing(self , plot : bool = False , saveToFile : bool = False): # foh = first order hold - do avg between the existing points
        assert self.phase == PHASE_TRAINING_STR, "This function is only available for training phase"
        pass

    def get_loss_ts(self , plot : bool = False , saveToFile : bool = False): # Todo change it
        """
        Returns a dictionary of {worker : loss list} for each worker in the experiment.
        use plot=True to plot the loss function.
        """
        assert self.phase == PHASE_TRAINING_STR, "This function is only available for training phase"
        loss_dict = {}
        workers_model_db_list = self.nerl_model_db.get_workers_model_db_list()
        workers_names = [worker_model_db.get_worker_name() for worker_model_db in workers_model_db_list]
        num_of_workers = len(workers_model_db_list)
        total_batches_list = [worker.get_total_batches() for worker in workers_model_db_list]
        max_batches = max(total_batches_list)
        batches_indexes = list(range(max_batches))
        
        for worker_db in workers_model_db_list:
            loss_dict[worker_db] = []
            for batch_timestamp in worker_db.batches_ts_dict.values().batch_timestamp:     #Todo Noa fix this line
                tansor_loss_data = worker_db.batches_ts_dict[batch_timestamp].get_tensor_data()
                loss_dict[worker_db][batch_timestamp] = tansor_loss_data


        data = np.zeros(shape=(len(batches_indexes),len(workers_model_db_list)))
        df = pd.DataFrame(data, columns = workers_names)
        print(df)
        pass




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
        
        # if saveToFile:
        #     export_dict_json(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/loss.json', loss_dict)
        # return loss_dict
    
    def get_loss_min(self , plot : bool = False , saveToFile : bool = False):  #Todo return get loss min and batch id
        """
        Returns a dictionary of {worker : min loss} for each worker in the experiment.
        use plot=True to plot the min loss of each worker.
        """
        min_loss_dict = OrderedDict()
        for key, loss_list in self.get_loss().items():
            min_loss_dict[key] = min(loss_list)
        if plot: # Plot in dots the min loss of each worker
            plt.figure(figsize = (30,15), dpi = 150)
            plt.rcParams.update({'font.size': 22})
            plt.plot(list(min_loss_dict.keys()), list(min_loss_dict.values()), 'o')
            plt.xlabel('Worker' , fontsize=30)
            plt.ylabel('Loss (MSE)' , fontsize=30)
            plt.yscale('log')
            plt.xlim(left=0)
            plt.ylim(bottom=0)
            plt.title('Training Min Loss')
            plt.grid(visible=True, which='major', linestyle='-')
            plt.minorticks_on()
            plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)
            plt.show()
            plt.savefig(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Training/Min_loss_graph.png')
            
        if saveToFile:
            export_dict_json(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/min_loss.json', min_loss_dict)
        return min_loss_dict

    def get_confusion_matrices(self , normalize : bool = False ,plot : bool = False , saveToFile : bool = False):
        """
        Returns a dictionary of {worker : {class : confusion matrix}} for each worker in the experiment.
        use plot=True to plot the confusion matrix.
        """
        workers_confusion_matrices = {}
        labels = self.experiment.get_labels_df()
        workersList = self.experiment.get_workers_list()
        if labels is None:
            raise "No labels file found , check your input data directory"
        workers_results = self.experiment.get_results_labels()
        if plot:
            f, axes = plt.subplots(len(workersList), self.experiment.labelsLen, figsize=(globe.MATRIX_DISP_SCALING*self.experiment.labelsLen, globe.MATRIX_DISP_SCALING*len(workersList)))
        for i, worker in enumerate(workersList):
            workers_confusion_matrices[worker] = [[] for i in range(self.experiment.labelsLen)]

            for j in range(self.experiment.labelsLen):
                # print(f"worker {worker}, has {len(workerNeuronRes[worker][TRUE_LABLE_IND])} labels, with {len(workerNeuronRes[worker][TRUE_LABLE_IND][j])} samples")
                # print(f"confusion {worker}:{j}, has is of {workerNeuronRes[worker][TRUE_LABLE_IND][j]}, {workerNeuronRes[worker][PRED_LABLE_IND][j]}")
                if normalize == True :
                    workers_confusion_matrices[worker][j] = confusion_matrix(workers_results[worker][globe.TRUE_LABLE_IND][j], workers_results[worker][globe.PRED_LABLE_IND][j], normalize='all')
                else:
                    workers_confusion_matrices[worker][j] = confusion_matrix(workers_results[worker][globe.TRUE_LABLE_IND][j], workers_results[worker][globe.PRED_LABLE_IND][j])

                if plot:
                    disp = ConfusionMatrixDisplay(workers_confusion_matrices[worker][j], display_labels=["X", self.experiment.labelNames[j]])
                    disp.plot(ax=axes[i, j], colorbar=False)
                    disp.ax_.set_title(f'{worker}, class #{j}\nAccuracy={round(accuracy_score(workers_results[worker][globe.TRUE_LABLE_IND][j], workers_results[worker][globe.PRED_LABLE_IND][j]), 3)}')
                    if i < len(workersList) - 1:
                        disp.ax_.set_xlabel('') 
                    if  j != 0:
                        disp.ax_.set_ylabel('') 
                    #disp.im_.colorbar.remove()  #remove individual colorbars
                    fileName = f'{self.experiment.name}_confusion_matrices'
                    disp.figure_.savefig(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Prediction/{fileName}.png')
        if plot:
            plt.subplots_adjust(wspace=1, hspace=0.15) 
            f.colorbar(disp.im_, ax=axes)
            plt.show()
            
        if saveToFile:
            export_dict_json(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/confusion_matrices.json', workers_confusion_matrices)
        return workers_confusion_matrices
    
    def get_model_performence_stats(self , confMatDict , show : bool = False , saveToFile : bool = False, printStats = False) -> dict:
        """
        Returns a dictionary of {worker : {class: {Performence_Stat : VALUE}}} for each worker and class in the experiment.
        Performence Statistics Available are: TN, FP, FN, TP, Accuracy, Balanced Accuracy, Precision, Recall, True Negative Rate, Informedness, F1
        """
        workers_accuracy = OrderedDict()
        for worker in confMatDict.keys():
            workers_accuracy[worker] = OrderedDict()
            for j, label_stats in enumerate(confMatDict[worker]): # Multi-Class
                workers_accuracy[worker][j] = OrderedDict()
                tn, fp, fn, tp = label_stats.ravel()
                if printStats:
                    LOG_INFO(f"worker {worker} label: {j} tn: {tn}, fp: {fp}, fn: {fn}, tp: {tp}")
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

                workers_accuracy[worker][j]['TN'] = tn
                workers_accuracy[worker][j]['FP'] = fp
                workers_accuracy[worker][j]['FN'] = fn
                workers_accuracy[worker][j]['TP'] = tp
                workers_accuracy[worker][j]['Accuracy'] = acc
                workers_accuracy[worker][j]['Balanced Accuracy'] = bacc
                workers_accuracy[worker][j]['Precision'] = ppv
                workers_accuracy[worker][j]['Recall'] = tpr
                workers_accuracy[worker][j]['True Negative Rate'] = tnr
                workers_accuracy[worker][j]['Informedness'] = inf
                workers_accuracy[worker][j]['F1'] = f1

                if show:
                    print(f"{worker}, class #{j}:")
                    print(f"{workers_accuracy[worker][j]}\n")
        
        if saveToFile:
            export_dict_json(f'{EXPERIMENT_RESULTS_PATH}/{self.exp_path}/accuracy_stats.json', workers_accuracy)
            
        return workers_accuracy
    

    def get_predict_regression_stats(self , plot : bool = False , saveToFile : bool = False):
        pass


