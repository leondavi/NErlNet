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
        main_server_args = values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] if values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] else None
        main_server_inst = MainServer(main_server_ip, main_server_port, main_server_args) if main_server_ip and main_server_port else None
        error_list.append(main_server_inst.error())

        api_server_ip = values[KEY_SETTINGS_APISERVER_IP_INPUT] if values[KEY_SETTINGS_APISERVER_IP_INPUT] else None
        api_server_port = values[KEY_SETTINGS_APISERVER_PORT_INPUT] if values[KEY_SETTINGS_APISERVER_PORT_INPUT] else None
        api_server_args = values[KEY_SETTINGS_APISERVER_ARGS_INPUT] if values[KEY_SETTINGS_APISERVER_ARGS_INPUT] else None
        api_server_inst = ApiServer(api_server_ip, api_server_port, api_server_args) if api_server_ip and api_server_port else None
        error_list.append(api_server_inst.error())

        nerlgui_server_inst = None
        if values[KEY_SETTINGS_NERLGUI_IP_INPUT] and values[KEY_SETTINGS_NERLGUI_PORT_INPUT] and values[KEY_CHECKBOX_ENABLE_NERLGUI]:
            nerlgui_server_ip = values[KEY_SETTINGS_NERLGUI_IP_INPUT] if values[KEY_SETTINGS_NERLGUI_IP_INPUT] else None
            nerlgui_server_port = values[KEY_SETTINGS_NERLGUI_PORT_INPUT] if values[KEY_SETTINGS_NERLGUI_PORT_INPUT] else None
            nerlgui_server_args = values[KEY_SETTINGS_NERLGUI_ARGS_INPUT] if values[KEY_SETTINGS_NERLGUI_ARGS_INPUT] else None
            nerlgui_server_inst = NerlGUI(nerlgui_server_ip, nerlgui_server_port, nerlgui_server_args)
            error_list.append(nerlgui_server_inst.error())
        
        error = any(error_list)
        if error:
            pass # pop up windows with issues

        json_architecture_instance.add_nerlnet_settings(frequency_inst, batch_size_inst)
        json_architecture_instance.add_main_server(main_server_inst)
        json_architecture_instance.add_api_server(api_server_inst)



def update_current_json_file_path(jsonPath):
    print(jsonPath)