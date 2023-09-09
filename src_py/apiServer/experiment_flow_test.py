
import os
from apiServer import *
from runCommand import RunCommand

def print_test(in_str : str):
    PREFIX = "[NERLNET-TEST] "
    print(f"{PREFIX} {in_str}")

NERLNET_PATH = os.getenv('NERLNET_PATH')
TESTS_PATH = os.getenv('TESTS_PATH')
NERLNET_RUN_SCRIPT = "./NerlnetRun.sh"
NERLNET_RUNNING_TIMEOUT_SEC = int(os.getenv('NERLNET_RUNNING_TIMEOUT_SEC'))

nerlnet_run_cmd = RunCommand(NERLNET_RUN_SCRIPT, NERLNET_PATH)


api_server_instance = ApiServer()
api_server_instance.help()

api_server_instance.showJsons()
api_server_instance.setJsons(0,0,0)

stdout, stderr, rc = nerlnet_run_cmd.sync(NERLNET_RUNNING_TIMEOUT_SEC)
print_test(rc)
print_test(stdout)
raise "break exception"

arch_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()

api_server_instance.initialization(arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.sendDataToSources("Training")
api_server_instance.train("test")

api_server_instance.sendDataToSources("Prediction")
api_server_instance.predict()

#api_server_instance.plot_loss(1)
#api_server_instance.accuracy_matrix(1)
#api_server_instance.statistics()
