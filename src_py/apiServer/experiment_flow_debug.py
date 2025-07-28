
import os
from apiServer import *
from logger import *
from stats import *
from runCommand import RunCommand
from definitions import *

TEST_DATASET_IDX = 2

def print_test(in_str : str):
    PREFIX = "[NERLNET-TEST] "
    LOG_INFO(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
NERLNET_RUN_SCRIPT = ""
NERLNET_RUN_STOP_SCRIPT = ""

api_server_instance = ApiServer()
api_server_instance.download_dataset(TEST_DATASET_IDX)
#api_server_instance.help()
api_server_instance.showJsons()
dc_idx = 0
conn_idx = 0
exp_idx = 0
api_server_instance.setJsons(dc_idx, conn_idx, exp_idx)
dc_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

experiment_name = "test_exp"
api_server_instance.initialization(experiment_name, dc_json , connmap_json, exp_flow_json) # start to debug
api_server_instance.send_jsons_to_devices()
 
api_server_instance.run_current_experiment_phase() # blocking - deppended acks from mainserver
stats = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
stats.get_communication_stats_workers()
stats.get_communication_stats_sources()
stats.get_communication_stats_clients()
stats.get_communication_stats_routers()
stats.get_communication_stats_main_server()
perf_stats = stats.get_performance_stats_clients()
print(pretty_dict(perf_stats))
stats.get_loss_ts()
stats.get_min_loss()
api_server_instance.next_experiment_phase()
api_server_instance.run_current_experiment_phase()
stats = api_server_instance.get_experiment_flow(experiment_name).generate_stats()
confusion_matrix_source_dict, confusion_matrix_worker_dict = stats.get_confusion_matrices()
performence_stats = stats.get_model_performence_stats(confusion_matrix_worker_dict, True)
stats.get_missed_batches()
perf_stats = stats.get_performance_stats_clients()
print(pretty_dict(perf_stats))
exit(0)
next_expertiment_phase_exist = api_server_instance.next_experiment_phase() 
api_server_instance.run_current_experiment_phase()
api_server_instance.communication_stats()
# next_expertiment_phase_exist = api_server_instance.next_experiment_phase()  # expected error


# api_server_instance.sendDataToSources(PHASE_TRAINING) # sync on ack
# api_server_instance.train() # sync on ack

# api_server_instance.sendDataToSources(PHASE_PREDICTION)
# api_server_instance.predict()
api_server_instance.communication_stats()

experiment_flow_inst = api_server_instance.get_experiment(experiment_name)
experiment_phase_inst = experiment_flow_inst.get_current_experiment_phase()
exp_stats = Stats(experiment_phase_inst)
exp_stats.get_loss_ts()
exit(0)
#exp_stats.get_loss_min(saveToFile=True)
#loss = exp_stats.get_loss()
#loss_min = exp_stats.get_loss_min()
conf_mats = exp_stats.get_confusion_matrices()
model_stats = exp_stats.get_model_performence_stats(conf_mats , show=True , saveToFile=True)
for worker in model_stats.keys():
    for j in model_stats[worker].keys():
        print(f'{worker} class {j} F1 Score: {model_stats[worker][j]["F1"]}')
baseline_model_stats = import_dict_json('/home/guyperets/Desktop/NErlNet/Tests/inputJsonsFiles/accuracy_stats_synt_1d_2c_4r_4w.json')
diff_from_baseline = []
for worker in model_stats.keys():
    for j in model_stats[worker].keys():
        diff = abs(model_stats[worker][j]["F1"] - baseline_model_stats[worker][str(j)]["F1"])
        diff_from_baseline.append(diff/baseline_model_stats[worker][str(j)]["F1"])
        
#print(f'Loss Dict: {loss}')
#print(f'Loss Min Dict: {loss_min}')
#print(f'Confusion Matrices: {conf}')
#api_server_instance.statistics() TODO change statistics input requests to API!
# TODO validation of statistics with baseline - margin up to 10%

#api_server_instance.plot_loss(1)
#api_server_instance.accuracy_matrix(1)
#api_server_instance.statistics()

# stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
# print_test(f'rc: {rc}')
# if stderr: 
#     print_test(stderr)
# else:
#     print_test(stdout)

# api_server_instance.stop() # TODO implement