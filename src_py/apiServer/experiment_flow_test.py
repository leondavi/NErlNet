
import os
from apiServer import *
from runCommand import RunCommand
from logger import *
from stats import Stats

ExitValue = 0

TEST_ACCEPTABLE_MARGIN_OF_ERROR = 0.03 # 3% marginal error

def print_test(in_str : str , enable = True):
    PREFIX = "[NERLNET-TEST] "
    if enable:
        LOG_INFO(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
TESTS_PATH = os.getenv('TESTS_PATH')
TESTS_BASELINE_MODEL_STATS = os.getenv('TEST_BASELINE_MODEL_STATS')
TEST_BASELINE_LOSS_MIN = os.getenv('TEST_BASELINE_LOSS_MIN')
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh --run-mode release"
NERLNET_RUN_STOP_SCRIPT = "./NerlnetRun.sh --run-mode stop"
NERLNET_RUNNING_TIMEOUT_SEC = int(os.getenv('NERLNET_RUNNING_TIMEOUT_SEC'))

WAIT_TIME_FOR_NERLNET_RUN_BOOT=60 # secs

# TODO JUST FOR DEBUG
print_test(f"$NERLNET_PATH: {NERLNET_PATH}")
print_test(f"$TESTS_PATH: {TESTS_PATH}")
print_test(f"$NERLNET_RUN_SCRIPT: {NERLNET_RUN_SCRIPT}")
print_test(f"$NERLNET_RUNNING_TIMEOUT_SEC: {NERLNET_RUNNING_TIMEOUT_SEC}")

print_test("NerlnetApp Start")
nerlnet_run_cmd = RunCommand(NERLNET_RUN_SCRIPT, NERLNET_PATH)
time.sleep(WAIT_TIME_FOR_NERLNET_RUN_BOOT) # TODO replace with keep alive loop

api_server_instance = ApiServer()
#api_server_instance.help()
#api_server_instance.showJsons()
api_server_instance.setJsons(0,0,0)

dc_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

experiment_name = "test_exp"
api_server_instance.initialization(experiment_name, dc_json , connmap_json, exp_flow_json)
api_server_instance.send_jsons_to_devices()

curr_experiment_phase_exists = api_server_instance.experiment_phase_is_valid()
assert curr_experiment_phase_exists, "No experiment phase found"

api_server_instance.run_current_experiment_phase() # blocking until phase is completed
api_server_instance.communication_stats()
stats_train = api_server_instance.get_experiment_flow(experiment_name).generate_stats()

next_expertiment_phase_exist = api_server_instance.next_experiment_phase()
assert next_expertiment_phase_exist, "No next experiment phase found"
api_server_instance.run_current_experiment_phase() # blocking until phase is completed
api_server_instance.communication_stats()
stats_predict = api_server_instance.get_experiment_flow(experiment_name).generate_stats()

print_test("Experiment phases completed")

print_test("Stopping NerlnetApp")
nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
print_test(f'rc: {rc}')
if stderr: 
    LOG_ERROR(stderr)
else:
    print_test(stdout)
stdout, stderr, rc = nerlnet_stop_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
print_test(f'rc stop: {rc}')
if stderr: 
    LOG_ERROR(stderr)
else:
    print_test(stdout, False)

generate_baseline_files = True

loss_min_dict = stats_train.get_min_loss(saveToFile=generate_baseline_files)
_ , confusion_matrix_worker_dict = stats_predict.get_confusion_matrices()
performence_stats = stats_predict.get_model_performence_stats(confusion_matrix_worker_dict, saveToFile=generate_baseline_files)

print(TEST_BASELINE_LOSS_MIN)
baseline_loss_min = import_dict_json(TEST_BASELINE_LOSS_MIN)
baseline_performance_stats = import_dict_pickle(TESTS_BASELINE_MODEL_STATS)

for worker in loss_min_dict.keys():
    diff = abs(loss_min_dict[worker] - baseline_loss_min[worker])
    print(f"worker: {worker}, loss: {loss_min_dict[worker]} loss baseline: {baseline_loss_min[worker]} diff: {diff}")
    if baseline_loss_min[worker] == 0:
        error = diff
    else:
        error = diff/baseline_loss_min[worker]
    LOG_INFO(f"worker: {worker}, diff: {diff} , error: {error}")
    if error > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
        LOG_ERROR(f"Anomaly failure detected")
        LOG_ERROR(f"Error: {error} , Acceptable error: {TEST_ACCEPTABLE_MARGIN_OF_ERROR}")
        ExitValue = 1
        break
        

DIFF_MEASURE_METHOD = "F1"

for worker in performence_stats.keys():
    for j in performence_stats[worker].keys():
        diff = abs(performence_stats[worker][j][DIFF_MEASURE_METHOD] - baseline_performance_stats[worker][str(j)][DIFF_MEASURE_METHOD])
        error = diff/baseline_performance_stats[worker][str(j)][DIFF_MEASURE_METHOD]
        if error > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
            LOG_ERROR("Anomaly failure detected")
            LOG_ERROR(f"diff_from_baseline: {diff}")
            ExitValue = 1
            break

exit(ExitValue)
