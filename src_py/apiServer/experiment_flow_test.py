
import os
from apiServer import *
from runCommand import RunCommand
from logger import *
from stats import Stats

ExitValue = 0

TEST_ACCEPTABLE_MARGIN_OF_ERROR = 0.01 # distance from loss value to baseline loss value
TEST_ACCEPTABLE_F1_DIFF = 0.02 # distance from F1 value to baseline F1 value

def print_test(in_str : str , enable = True):
    PREFIX = "[NERLNET-TEST] "
    if enable:
        LOG_INFO(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
TESTS_PATH = os.getenv('TESTS_PATH')
TESTS_BASELINE_MODEL_STATS = os.getenv('TEST_BASELINE_MODEL_STATS')
TEST_BASELINE_LOSS_MIN = os.getenv('TEST_BASELINE_LOSS_MIN')
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh --run-mode release > /tmp/nerlnet_run_log.txt 2>&1"
NERLNET_RUN_STOP_SCRIPT = "./NerlnetRun.sh --run-mode stop"
NERLNET_RUNNING_TIMEOUT_SEC = int(os.getenv('NERLNET_RUNNING_TIMEOUT_SEC'))
TEST_DATASET_IDX = 2

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
api_server_instance.download_dataset(TEST_DATASET_IDX)
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
stats_train = api_server_instance.get_experiment_flow(experiment_name).generate_stats()

api_server_instance.next_experiment_phase()
assert api_server_instance.next_expertiment_phase_exist, "No next experiment phase found"
api_server_instance.run_current_experiment_phase() # blocking until phase is completed
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


LOG_INFO("Communication stats training:")

comm_stats_str = f"main server: {stats_train.get_communication_stats_main_server()}\
workers: {stats_train.get_communication_stats_workers()}\
sources: {stats_train.get_communication_stats_sources()}\
clients: {stats_train.get_communication_stats_clients()}\
routers: {stats_train.get_communication_stats_routers()}"

LOG_INFO("Actual Frequencies:")
print(f"{stats_train.get_actual_frequencies_of_sources()}")

LOG_INFO("Missed Batches training:")
#LOG_INFO(stats_train.get_missed_batches())

LOG_INFO("Communication stats prediction:")
comm_stats_str = f"main server: {stats_predict.get_communication_stats_main_server()}\
workers: {stats_predict.get_communication_stats_workers()}\
sources: {stats_predict.get_communication_stats_sources()}\
clients: {stats_predict.get_communication_stats_clients()}\
routers: {stats_predict.get_communication_stats_routers()}"

LOG_INFO("Actual Frequencies:")
print(f"{stats_predict.get_actual_frequencies_of_sources()}")

missed_batches = stats_predict.get_missed_batches()
if missed_batches:
    LOG_INFO("Missed Batches prediction:")
    LOG_INFO(missed_batches)

generate_baseline_files = True

loss_min_dict = stats_train.get_min_loss(saveToFile=generate_baseline_files)
LOG_INFO(loss_min_dict)
_ , confusion_matrix_worker_dict = stats_predict.get_confusion_matrices()
performence_stats = stats_predict.get_model_performence_stats(confusion_matrix_worker_dict, saveToFile=generate_baseline_files) # Now a pandas DataFrame

baseline_loss_min = import_dict_json(TEST_BASELINE_LOSS_MIN)
baseline_performance_stats = import_csv_df(TESTS_BASELINE_MODEL_STATS)

baseline_loss_min_avg = average_list(list(baseline_loss_min.values()))

for worker in loss_min_dict.keys():
    dist_from_avg_anomaly = abs(loss_min_dict[worker] - baseline_loss_min_avg)
    if dist_from_avg_anomaly > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
        LOG_INFO(f"Anomaly: {dist_from_avg_anomaly}, error: {loss_min_dict[worker]} , baseline mean: {baseline_loss_min_avg}, Acceptable error range: {TEST_ACCEPTABLE_MARGIN_OF_ERROR}")
        LOG_ERROR(f"Anomaly failure detected")
        ExitValue = 1
        

DIFF_MEASURE_METHOD = "F1"
        
for f1_score_exp , f1_score_baseline in zip(performence_stats[DIFF_MEASURE_METHOD], baseline_performance_stats[DIFF_MEASURE_METHOD]):
    diff = abs(f1_score_exp - f1_score_baseline)
    error = diff/f1_score_baseline
    if error > TEST_ACCEPTABLE_F1_DIFF:
        LOG_INFO(f"Anomaly: {error}, Diff: {diff}, F1: {f1_score_exp} , F1 baseline: {f1_score_baseline}, Acceptable error range: {TEST_ACCEPTABLE_F1_DIFF}")
        LOG_ERROR("Anomaly failure detected")
        LOG_ERROR(f"diff_from_baseline: {diff}")
        ExitValue = 1

exit(ExitValue)
