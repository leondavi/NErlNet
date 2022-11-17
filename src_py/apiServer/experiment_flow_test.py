
from apiServer import *
from experiment import *

api_server_instance = ApiServer()

api_server_instance.showJsons()

api_server_instance.selectJsons()
print()
print(api_server_instance.getUserJsons())

arch_json , connmap_json, exp_flow_json = api_server_instance.getUserJsons()


api_server_instance.initialization(arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.train()
api_server_instance.predict()
api_server_instance.statistics()