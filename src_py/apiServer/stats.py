from collections import OrderedDict
from sklearn.metrics import confusion_matrix, ConfusionMatrixDisplay, accuracy_score
import matplotlib.pyplot as plt
from pathlib import Path
from experiment import Experiment
import globalVars as globe
from definitions import *

class Stats():
    def __init__(self, experiment : Experiment):
        self.experiment = experiment
        #self.labels = self.experiment.get_labels_df()
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Training').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Prediction').mkdir(parents=True, exist_ok=True)

    def get_loss(self , plot : bool = False):
        """
        Returns a dictionary of {worker : loss list} for each worker in the experiment.
        use plot=True to plot the loss function.
        """
        workers_res_dict = {}
        for csvRes in self.experiment.trainingResList:
            for workerRes in csvRes.workersResList:
                worker_name = workerRes.get_name()
                workers_res_dict[worker_name] = workerRes.resList
        loss_dict = OrderedDict(sorted(workers_res_dict.items()))
        if plot: 
            plt.figure(figsize = (30,15), dpi = 150)
            plt.rcParams.update({'font.size': 22})
            for worker_name, loss_list in loss_dict.items():
                plt.plot(loss_list, label=worker_name)
            plt.legend(list(loss_dict.keys()))
            plt.xlabel('Batch Number' , fontsize=30)
            plt.ylabel('Loss (MSE)' , fontsize=30)
            plt.yscale('log')
            plt.xlim(left=0)
            plt.ylim(bottom=0)
            plt.title('Training Loss Function')
            plt.grid(visible=True, which='major', linestyle='-')
            plt.minorticks_on()
            plt.grid(visible=True, which='minor', linestyle='-', alpha=0.7)
            plt.show()
            plt.savefig(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Training/Loss_graph.png')
        return loss_dict
    
    def get_loss_min(self , plot : bool = False):
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
        return min_loss_dict

    def get_confusion_matrices(self , normalize : bool = False ,plot : bool = False):
        """
        Returns a dictionary of {worker : confusion matrix} for each worker in the experiment.
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
        return workers_confusion_matrices
    
    def get_accuracy_stats(self , confMatDict , show : bool = False) -> dict:
        """
        Returns a dictionary of {worker : accuracy} for each worker in the experiment.
        """
        statFileName = f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Prediction/stats.txt'
        if os.path.exists(statFileName): 
            os.remove(statFileName)
        statFile = open(statFileName, "a")
        workers_accuracy = {worker : {} for worker in confMatDict.keys()}
        for worker in confMatDict.keys():
            for j, label in enumerate(confMatDict[worker]):
                tn, fp, fn, tp = label.ravel()
                acc = (tp + tn) / (tp + tn + fp + fn)
                ppv = tp / (tp + fp)
                tpr = tp / (tp + fn)
                tnr = tn / (tn + fp)
                bacc = (tpr + tnr) / 2
                inf = tpr + tnr - 1

                workers_accuracy[worker]['TN'] = tn
                workers_accuracy[worker]['FP'] = fp
                workers_accuracy[worker]['FN'] = fn
                workers_accuracy[worker]['TP'] = tp
                workers_accuracy[worker]['Accuracy'] = acc
                workers_accuracy[worker]['Balanced Accuracy'] = bacc
                workers_accuracy[worker]['Positive Predictive Rate'] = ppv
                workers_accuracy[worker]['True Positive Rate'] = tpr
                workers_accuracy[worker]['True Negative Rate'] = tnr
                workers_accuracy[worker]['Informedness'] = inf

            statFile.write(f"{worker}, class #{j}:\n")
            statFile.write(f"Accuracy acquired (TP+TN / Tot):            {round(acc*100, 3)}%.\n")
            statFile.write(f"Balanced Accuracy (TPR+TNR / 2):            {round(bacc*100, 3)}%.\n")
            statFile.write(f"Positive Predictive Rate (Precision of P):  {round(ppv*100, 3)}%.\n")
            statFile.write(f"True Pos Rate (Sensitivity / Hit Rate):     {round(tpr*100, 3)}%.\n")
            statFile.write(f"True Neg Rate (Selectivity):                {round(tnr*100, 3)}%.\n")
            statFile.write(f"Informedness (of making decision):          {round(inf*100, 3)}%.\n")

            if show:
                print(f"{worker}, class #{j}:")
                print(f"Accuracy acquired (TP+TN / Tot):            {round(acc*100, 3)}%.")
                print(f"Balanced Accuracy (TPR+TNR / 2):            {round(bacc*100, 3)}%.")
                print(f"Positive Predictive Rate (Precision of P):  {round(ppv*100, 3)}%.")
                print(f"True Pos Rate (Sensitivity / Hit Rate):     {round(tpr*100, 3)}%.")
                print(f"True Neg Rate (Selectivity):                {round(tnr*100, 3)}%.")
                print(f"Informedness (of making decision):          {round(inf*100, 3)}%.\n\n")
        statFile.close()
        return workers_accuracy

