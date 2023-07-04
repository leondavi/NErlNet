from JsonElements import *
from JsonArchitecture import JsonArchitecture
from Definitions import *

# instances and lists of instances
json_architecture_instance = JsonArchitecture()

# instances
batch_size = None
main_server_inst = None
api_server_inst = None
nerl_gui_inst = None

def reset_instances():
    json_architecture_instance = JsonArchitecture()

def settings_handler(event, values):
    frequency = None
    frequency_inst = None
    error_list = []
    if event == KEY_SETTINGS_ADD_BUTTON:
        frequency = values[KEY_SETTINGS_FREQUENCY_INPUT] if values[KEY_SETTINGS_FREQUENCY_INPUT] else None
        frequency_inst = Frequency(frequency) if frequency else None
        error_list.append(frequency_inst.error())
        print(f"Frequency={frequency}") #TODO remove - jsut debug

        batch_size = values[KEY_SETTINGS_BATCH_SIZE_INPUT] if values[KEY_SETTINGS_BATCH_SIZE_INPUT] else None
        batch_size_inst = BatchSize(batch_size) if batch_size else None
        error_list.append(batch_size_inst.error())
        print(f"batch_size={batch_size}") #TODO remove - jsut debug

        main_server_ip = values[KEY_SETTINGS_MAINSERVER_IP_INPUT] if values[KEY_SETTINGS_MAINSERVER_IP_INPUT] else None
        main_server_port = values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] if values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] else None
        main_server_inst = MainServer(main_server_ip, main_server_port)
        error_list.append(main_server_inst.error())

        api_server_ip = values[KEY_SETTINGS_APISERVER_IP_INPUT] if values[KEY_SETTINGS_APISERVER_IP_INPUT] else None
        api_server_port = values[KEY_SETTINGS_APISERVER_PORT_INPUT] if values[KEY_SETTINGS_APISERVER_PORT_INPUT] else None
        api_server_inst = MainServer(api_server_ip, api_server_port)
        error_list.append(api_server_inst.error())

        if values[KEY_SETTINGS_NERLGUI_IP_INPUT] and values[KEY_SETTINGS_NERLGUI_PORT_INPUT]:
            nerlgui_server_ip = values[KEY_SETTINGS_NERLGUI_IP_INPUT] if values[KEY_SETTINGS_NERLGUI_IP_INPUT] else None
            nerlgui_server_port = values[KEY_SETTINGS_NERLGUI_PORT_INPUT] if values[KEY_SETTINGS_NERLGUI_PORT_INPUT] else None
            nerlgui_server_inst = MainServer(nerlgui_server_ip, nerlgui_server_port)
            error_list.append(nerlgui_server_inst.error())
        
        error = any(error_list)
        if error:
            pass # pop up windows with issues



def update_current_json_file_path(jsonPath):
    print(jsonPath)