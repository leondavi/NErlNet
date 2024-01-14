
import os
from apiServer import *
from runCommand import RunCommand
from logger import *
from stats import Stats

TEST_ACCEPTABLE_MARGIN_OF_ERROR = 0.02

def print_test(in_str : str):
    PREFIX = "[NERLNET-TEST] "
    LOG_INFO(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
TESTS_PATH = os.getenv('TESTS_PATH')
TESTS_BASELINE_MODEL_STATS = os.getenv('TEST_BASELINE_MODEL_STATS')
TESTS_BASELINE_LOSS_MIN = os.getenv('TEST_BASELINE_LOSS_MIN')
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

arch_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

experiment_name = "test_exp"
api_server_instance.initialization(experiment_name, arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.sendDataToSources(PHASE_TRAINING)
api_server_instance.train()

api_server_instance.sendDataToSources(PHASE_PREDICTION)
api_server_instance.predict()

experiment_inst = api_server_instance.get_experiment(experiment_name)
nerlnet_stop_cmd = RunCommand(NERLNET_RUN_STOP_SCRIPT, NERLNET_PATH)
stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
print_test(f'rc: {rc}')
if stderr: 
    print_test(stderr)
else:
    print_test(stdout)
stdout, stderr, rc = nerlnet_stop_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
print_test(f'rc stop: {rc}')
if stderr: 
    print_test(stderr)
else:
    print_test(stdout)

exp_stats = Stats(experiment_inst)
loss_min = exp_stats.get_loss_min(saveToFile=True)
print_test("min loss of each worker")
baseline_loss_min = import_dict_json(TESTS_BASELINE_LOSS_MIN)
for worker in loss_min.keys():
    diff = abs(loss_min[worker] - baseline_loss_min[worker])
    if baseline_loss_min[worker] == 0:
        error = diff
    else:
        error = diff/baseline_loss_min[worker]
    print_test(f"worker: {worker}, diff: {diff} , error: {error}")
    if error > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
        print(f"Anomaly failure detected")
        print(f"Error: {error} , Acceptable error: {TEST_ACCEPTABLE_MARGIN_OF_ERROR}")
        exit(1)
        


conf_mats = exp_stats.get_confusion_matrices()
performence_stats = exp_stats.get_model_performence_stats(conf_mats, saveToFile=True, printStats=True)
baseline_acc_stats = import_dict_json(TESTS_BASELINE_MODEL_STATS)
for worker in performence_stats.keys():
    for j in performence_stats[worker].keys():
        diff = abs(performence_stats[worker][j]["F1"] - baseline_acc_stats[worker][str(j)]["F1"])
        error = diff/baseline_acc_stats[worker][str(j)]["F1"]
        if error > TEST_ACCEPTABLE_MARGIN_OF_ERROR:
            print_test("Anomaly failure detected")
            print_test(f"diff_from_baseline: {diff}")
            exit(1)
