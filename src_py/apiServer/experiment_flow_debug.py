
import os
from apiServer import *
from logger import *
from stats import Stats
from runCommand import RunCommand
from definitions import *

def print_test(in_str : str):
    PREFIX = "[NERLNET-TEST] "
    LOG_INFO(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh --run-mode release"
NERLNET_RUN_STOP_SCRIPT = "./NerlnetRun.sh --run-mode stop"

api_server_instance = ApiServer()
#api_server_instance.help()
api_server_instance.showJsons()
arch = 3
conn = 15
flow = 10
api_server_instance.setJsons(arch, conn, flow)
arch_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

experiment_name = "test_exp"
api_server_instance.initialization(experiment_name, arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.sendDataToSources(PHASE_TRAINING)
api_server_instance.train()

api_server_instance.sendDataToSources(PHASE_PREDICTION)
api_server_instance.predict()

experiment_inst = api_server_instance.get_experiment(experiment_name)
exp_stats = Stats(experiment_inst)
loss = exp_stats.get_loss()
loss_min = exp_stats.get_loss_min()
conf = exp_stats.get_confusion_matrices()
stats = exp_stats.get_accuracy_stats(conf)
#print(f'Loss Dict: {loss}')
#print(f'Loss Min Dict: {loss_min}')
#print(f'Confusion Matrices: {conf}')
#api_server_instance.statistics() TODO change statistics input requests to API!
# TODO validation of statistics with baseline - margin up to 10%

#api_server_instance.plot_loss(1)
#api_server_instance.accuracy_matrix(1)
#api_server_instance.statistics()

nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
# stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
# print_test(f'rc: {rc}')
# if stderr: 
#     print_test(stderr)
# else:
#     print_test(stdout)
stdout, stderr, rc = nerlnet_stop_cmd.sync(0)
print_test(f'rc stop: {rc}')
if stderr: 
    print_test(stderr)
else:
    print_test(stdout)

# api_server_instance.stop() # TODO implement