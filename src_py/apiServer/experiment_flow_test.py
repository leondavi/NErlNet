
from jsonDirParser import JsonDirParser
from apiServer.apiServer import *

json_dir_parser = JsonDirParser()
json_dir_parser.print_lists()
json_dir_parser.select_arch_connmap_experiment(0,1,1)
print()
print(json_dir_parser.get_user_selection_files())

arch_path , connmap_path, exp_path = json_dir_parser.get_user_selection_files()

instance = ApiServer()
apiServerInst.initialization(arch_path , connmap_path, exp_path)
instance.sendJsonsToDevices()