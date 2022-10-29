
from jsonDirParser import JsonDirParser
from apiServer import *
from experiment import *

json_dir_parser = JsonDirParser()
json_dir_parser.print_lists()
json_dir_parser.select_arch_connmap_experiment(0,1,1)
print()
print(json_dir_parser.get_user_selection_files())

arch_json , connmap_json, exp_flow_json = json_dir_parser.get_user_selection_jsons()

api_server_instance = ApiServer()

api_server_instance.initialization(arch_json , connmap_json, exp_flow_json)
api_server_instance.sendJsonsToDevices()

api_server_instance.train()
api_server_instance.predict()
api_server_instance.statistics()