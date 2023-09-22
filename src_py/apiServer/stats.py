from collections import OrderedDict
from experiment import Experiment
from pathlib import Path

from definitions import *

class Stats():
    def __init__(self, experiment : Experiment):
        self.experiment = experiment
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Training').mkdir(parents=True, exist_ok=True)
        Path(f'{EXPERIMENT_RESULTS_PATH}/{self.experiment.name}/Prediction').mkdir(parents=True, exist_ok=True)

    def get_loss(self):
        workers_res_dict = {}
        for csvRes in self.experiment.trainingResList:
            for workerRes in csvRes.workersResList:
                worker_name = workerRes.get_name()
                workers_res_dict[worker_name] = workerRes.resList
        return OrderedDict(sorted(workers_res_dict.items()))
    
    def get_loss_min(self):
        workers_res_dict = OrderedDict()
        for key, loss_list in self.get_loss().items():
            workers_res_dict[key] = min(loss_list)
        return workers_res_dict