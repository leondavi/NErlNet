
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
TESTS_BASELINE = os.getenv('TESTS_BASELINE')
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
data = exp_stats.get_loss_min()
print_test("min loss of each worker")
print(data)

conf = exp_stats.get_confusion_matrices()
acc_stats = exp_stats.get_accuracy_stats(conf)
baseline_acc_stats = import_dict_json(TESTS_BASELINE)
diff_from_baseline = []
for worker in acc_stats.keys():
    for j in acc_stats[worker].keys():
        diff = abs(acc_stats[worker][j]["F1"] - baseline_acc_stats[worker][str(j)]["F1"])
        diff_from_baseline.append(diff/baseline_acc_stats[worker][str(j)]["F1"])
anomaly_detected = not all([x < TEST_ACCEPTABLE_MARGIN_OF_ERROR for x in diff_from_baseline])
if anomaly_detected:
    print_test("Anomaly failure detected")
    print_test(f"diff_from_baseline: {diff_from_baseline}")
    exit(1)

