from JsonElements import *
from JsonArchitecture import JsonArchitecture

# instances and lists of instances
json_architecture_instance = JsonArchitecture()

# instances
frequency = None
batch_size = None
main_server_inst = None
api_server_inst = None
nerl_gui_inst = None

def reset_instances():
    json_architecture_instance = JsonArchitecture()

def update_current_json_file_path(jsonPath):
    print(jsonPath)