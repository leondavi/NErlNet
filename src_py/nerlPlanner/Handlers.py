import json

from collections import OrderedDict
from JsonElements import *
from JsonElementWorker import *
from JsonArchitecture import JsonArchitecture
from Definitions import *

import PySimpleGUI as sg

# instances and lists of instances
json_architecture_instance = JsonArchitecture()

# instances
batch_size = None
main_server_inst = None
api_server_inst = None
nerl_gui_inst = None

# workers
workers_load_worker_path = None
workers_new_worker = None
workers_new_worker_name = None
workers_new_worker_dict = None
worker_name_selection = None

# devices
device_name = None
device_ip = None

# clients
clients_combo_box_worker_selection = None
this_client_name = None
this_client_port = None
this_client = None

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
        if nerlgui_server_inst is not None:
            json_architecture_instance.add_nerlgui_server(nerlgui_server_inst)



def workers_handler(window, event, values):
    global workers_load_worker_path
    global workers_new_worker
    global workers_new_worker_name
    global workers_new_worker_dict
    global worker_name_selection

    if event == KEY_WORKERS_INPUT_LOAD_WORKER_PATH:
        workers_load_worker_path = values[KEY_WORKERS_INPUT_LOAD_WORKER_PATH]
        with open(workers_load_worker_path) as jsonFile:
                workers_new_worker_dict = json.load(jsonFile)
        (workers_new_worker , _, _, _, _, _, _, _, _, _, _, _, _) = Worker.load_from_dict(workers_new_worker_dict)
        window[KEY_WORKERS_INFO_BAR].update(f'loaded from file: {workers_new_worker}')
        window[KEY_WORKERS_LIST_BOX].update(json_architecture_instance.get_workers_names_list())

    if event == KEY_WORKERS_NAME_INPUT:
        workers_new_worker_name = values[KEY_WORKERS_NAME_INPUT] if values[KEY_WORKERS_NAME_INPUT] not in json_architecture_instance.get_workers_dict() else None

    if workers_new_worker_name and (workers_new_worker_name not in json_architecture_instance.get_workers_dict()) and\
       (workers_new_worker is not None):
        workers_new_worker.set_name(workers_new_worker_name)
        
    if event == KEY_WORKERS_BUTTON_ADD and (workers_new_worker is not None):
        if not workers_new_worker.get_name():
            sg.popup_ok(f"Cannot add - Name is missing!", keep_on_top=True, title="Loading Issue")
        elif json_architecture_instance.add_worker(workers_new_worker):
            window[KEY_WORKERS_LIST_BOX].update(json_architecture_instance.get_workers_names_list())
            # Clear fields after successful add
            window[KEY_WORKERS_INFO_BAR].update(f'{workers_new_worker_name} added, {workers_new_worker}')
            workers_new_worker_name = ''
            window[KEY_WORKERS_NAME_INPUT].update(workers_new_worker_name)
            workers_load_worker_path = ''
            window[KEY_WORKERS_INPUT_LOAD_WORKER_PATH].update(workers_load_worker_path)
            workers_new_worker = None

    if event == KEY_WORKERS_LIST_BOX:
        worker_name_selection = values[KEY_WORKERS_LIST_BOX][0]
        window[KEY_WORKERS_INFO_BAR].update(f'{worker_name_selection} is selected')

    if event == KEY_WORKERS_LOAD_FROM_LIST_WORKER_BUTTON:
        if (worker_name_selection in json_architecture_instance.get_workers_dict()) and workers_new_worker_name:
            workers_new_worker = json_architecture_instance.get_workers_dict()[worker_name_selection].copy(workers_new_worker_name)
            window[KEY_WORKERS_INFO_BAR].update(f'{workers_new_worker_name} loaded, {workers_new_worker}')
        else:
            sg.popup_ok(f"selection or name issue", keep_on_top=True, title="Loading Issue")

    if event == KEY_WORKERS_SHOW_WORKER_BUTTON:
        if (worker_name_selection in json_architecture_instance.get_workers_dict()):
            workers_new_worker = json_architecture_instance.get_workers_dict()[worker_name_selection]
            sg.popup_ok(pretty_print_dict(workers_new_worker.get_as_dict(False)), keep_on_top=True, title="Worker Params")

def devices_handler(window, event, values):
    device_name = values[KEY_DEVICES_NAME_INPUT]
    device_ip = values[KEY_DEVICES_IP_INPUT]


def clients_handler(window, event, values):
    global clients_combo_box_worker_selection
    global this_client_name
    global this_client

    # update worker with list
    window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update(values[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX],values=list(json_architecture_instance.get_workers_names_list()))

    if event == KEY_CLIENTS_NAME_INPUT:
        this_client_name = values[KEY_CLIENTS_NAME_INPUT]

    if event == KEY_CLIENTS_PORT_INPUT:
        this_client_port = values[KEY_CLIENTS_NAME_INPUT]

    if event == KEY_CLIENTS_WORKERS_LIST_COMBO_BOX:
        clients_combo_box_worker_selection = values[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX]

    if event == KEY_CLIENTS_WORKERS_LIST_ADD_WORKER:
        pass

def update_current_json_file_path(jsonPath):
    print(jsonPath)