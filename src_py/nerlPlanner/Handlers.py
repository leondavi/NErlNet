import json
from collections import OrderedDict
import PySimpleGUI as sg

from JsonElements import *
from JsonElementWorker import *
from JsonDistributedConfig import JsonDistributedConfig
from Definitions import *
from HandlersGlobals import *


# instances and lists of instances
json_dc_inst = JsonDistributedConfig()

def reset_json_distributed_configuration():
    global json_dc_inst
    json_dc_inst = JsonDistributedConfig()

def settings_reset_inputs_ui(window):
    window[KEY_SETTINGS_FREQUENCY_INPUT].update('')
    window[KEY_SETTINGS_BATCH_SIZE_INPUT].update('')
    
def settings_special_entities_reset_inputs_ui(window):
    window[KEY_SETTINGS_MAINSERVER_PORT_INPUT].update('')
    window[KEY_SETTINGS_MAINSERVER_ARGS_INPUT].update('')
    window[KEY_SETTINGS_APISERVER_PORT_INPUT].update('')
    window[KEY_SETTINGS_APISERVER_ARGS_INPUT].update('')

def settings_handler(window, event, values):
    frequency = None
    frequency_inst = None
    error_list = []

    if event == KEY_SETTINGS_SAVE_BUTTON:
        frequency = values[KEY_SETTINGS_FREQUENCY_INPUT] if values[KEY_SETTINGS_FREQUENCY_INPUT] else None
        frequency_inst = Frequency(frequency) if frequency else None
        error_list.append(frequency_inst.error()) if frequency else error_list.append(True)

        batch_size = values[KEY_SETTINGS_BATCH_SIZE_INPUT] if values[KEY_SETTINGS_BATCH_SIZE_INPUT] else None
        batch_size_inst = BatchSize(batch_size) if batch_size else None
        error_list.append(batch_size_inst.error()) if batch_size else error_list.append(True)

        window[KEY_SETTINGS_STATUS_BAR].update(settings_freq_batch_str(frequency_inst.get_value_str(),batch_size_inst.get_value_str()))

        error = any(error_list)
        if error:
            sg.popup_ok(f"Wrong or missed input!", keep_on_top=True, title="Settings Input Issue")
        else:
            json_dc_inst.add_nerlnet_settings(frequency_inst, batch_size_inst)
            
    if event == KEY_SETTINGS_CLEAR_BUTTON:
        frequency = values[KEY_SETTINGS_FREQUENCY_INPUT] if values[KEY_SETTINGS_FREQUENCY_INPUT] else None
        batch_size = values[KEY_SETTINGS_BATCH_SIZE_INPUT] if values[KEY_SETTINGS_BATCH_SIZE_INPUT] else None
        if frequency and batch_size:
            json_dc_inst.clear_nerlnet_settings()
        settings_reset_inputs_ui(window)
    
    if event == KEY_SETTINGS_SPECIAL_ENTITIES_SAVE:
        main_server_port = values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] if values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] else None
        main_server_args = values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] if values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] else None
        main_server_inst = MainServer(main_server_port, main_server_args) if main_server_port else None
        error_list.append(main_server_inst.error()) if main_server_inst is not None else error_list.append(True)

        api_server_port = values[KEY_SETTINGS_APISERVER_PORT_INPUT] if values[KEY_SETTINGS_APISERVER_PORT_INPUT] else None
        api_server_args = values[KEY_SETTINGS_APISERVER_ARGS_INPUT] if values[KEY_SETTINGS_APISERVER_ARGS_INPUT] else None
        api_server_inst = ApiServer(api_server_port, api_server_args) if api_server_port else None
        error_list.append(api_server_inst.error()) if api_server_inst is not None else error_list.append(True)
        
        error = any(error_list)
        if error:
            sg.popup_ok(f"Wrong or missed input!", keep_on_top=True, title="Settings Input Issue")
        else:
            json_dc_inst.add_main_server(main_server_inst)
            json_dc_inst.add_api_server(api_server_inst)
            device_by_main_server = json_dc_inst.get_device_by_entity(main_server_inst.NAME)
            device_by_api_server = json_dc_inst.get_device_by_entity(api_server_inst.NAME)
            window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}, {device_by_main_server}")   
            window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}, {device_by_api_server}")

    if event == KEY_SETTINGS_SPECIAL_ENTITIES_CLEAR:
        main_server_port = values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] if values[KEY_SETTINGS_MAINSERVER_PORT_INPUT] else None
        main_server_args = values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] if values[KEY_SETTINGS_MAINSERVER_ARGS_INPUT] else None
        api_server_port = values[KEY_SETTINGS_APISERVER_PORT_INPUT] if values[KEY_SETTINGS_APISERVER_PORT_INPUT] else None
        api_server_args = values[KEY_SETTINGS_APISERVER_ARGS_INPUT] if values[KEY_SETTINGS_APISERVER_ARGS_INPUT] else None
        if main_server_port and main_server_args and api_server_port and api_server_args:
            json_dc_inst.clear_nerlnet_special_entities_settings()
        settings_special_entities_reset_inputs_ui(window)
        
def workers_handler(window, event, values):
    global workers_load_worker_path
    global workers_new_worker
    global workers_new_worker_name
    global workers_new_worker_dict
    global worker_name_selection
    global last_workers_list_state
    global last_workers_list_state_not_occupied
    global clients_combo_box_worker_selection
    
    if last_workers_list_state != json_dc_inst.get_workers_names_list():
        last_workers_list_state = json_dc_inst.get_workers_names_list()
        last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
        window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)

    if event == KEY_WORKERS_SHOW_WORKER_BUTTON:
        if (worker_name_selection in json_dc_inst.get_workers_dict()):
            workers_new_worker = json_dc_inst.get_workers_dict()[worker_name_selection]
        if workers_new_worker is not None:
            image_path = workers_new_worker.save_graphviz(NERLNET_GRAPHVIZ_OUTPUT_DIR)
            sg.popup_ok(f"{workers_new_worker}", title="Worker graph", image=image_path, keep_on_top=True)

    if event == KEY_WORKERS_INPUT_LOAD_WORKER_PATH:
        workers_load_worker_path = values[KEY_WORKERS_INPUT_LOAD_WORKER_PATH]
        with open(workers_load_worker_path) as jsonFile:
                workers_new_worker_dict = json.load(jsonFile)
        workers_new_worker = Worker.load_from_dict(workers_new_worker_dict)
        window[KEY_WORKERS_INFO_BAR].update(f'Loaded from file: {workers_new_worker}')
        window[KEY_WORKERS_LIST_BOX].update(json_dc_inst.get_workers_names_list())

    if event == KEY_WORKERS_NAME_INPUT:
        workers_new_worker_name = values[KEY_WORKERS_NAME_INPUT] if values[KEY_WORKERS_NAME_INPUT] not in json_dc_inst.get_workers_dict() else None

    if workers_new_worker_name and (workers_new_worker_name not in json_dc_inst.get_workers_dict()) and\
       (workers_new_worker is not None):
        workers_new_worker.set_name(workers_new_worker_name)
        
    if event == KEY_WORKERS_BUTTON_ADD and (workers_new_worker is not None):
        if workers_new_worker.get_name() and workers_new_worker.get_name()[0].isupper():
            sg.popup_ok(f"Worker name must start with lower case letter", title='Adding Worker Failed')
        elif not workers_new_worker.get_name():
            sg.popup_ok(f"Cannot add - Name is missing or exist!", keep_on_top=True, title="Adding New Worker Issue")
        elif json_dc_inst.add_worker(workers_new_worker):
            window[KEY_WORKERS_LIST_BOX].update(json_dc_inst.get_workers_names_list())
            # Clear fields after successful add
            window[KEY_WORKERS_INFO_BAR].update(f'{workers_new_worker_name} added, {workers_new_worker}')
            workers_new_worker_name = ''
            window[KEY_WORKERS_NAME_INPUT].update(workers_new_worker_name)
            workers_load_worker_path = ''
            window[KEY_WORKERS_INPUT_LOAD_WORKER_PATH].update(workers_load_worker_path)
            workers_new_worker = None
        else:
            sg.popup_ok(f"Cannot add - Parameters Issue!", keep_on_top=True, title="Adding New Worker Issue")


    if event == KEY_WORKERS_LIST_BOX:
        if values[KEY_WORKERS_LIST_BOX]:
            worker_name_selection = values[KEY_WORKERS_LIST_BOX][0]
            window[KEY_WORKERS_INFO_BAR].update(f'{worker_name_selection} is selected')

    if event == KEY_WORKERS_LOAD_FROM_LIST_WORKER_BUTTON:
        if (worker_name_selection in json_dc_inst.get_workers_dict()):
            workers_new_worker = json_dc_inst.get_workers_dict()[worker_name_selection]
            workers_new_worker_name = workers_new_worker.get_name()
            window[KEY_WORKERS_INFO_BAR].update(f'{workers_new_worker_name} loaded, {workers_new_worker}')
            window[KEY_WORKERS_NAME_INPUT].update(workers_new_worker_name)
        else:
            sg.popup_ok(f"selection or name issue", keep_on_top=True, title="Loading Issue")


def devices_reset_inputs_ui(window):
    window[KEY_DEVICES_NAME_INPUT].update('')
    window[KEY_DEVICES_IP_INPUT].update('x.x.x.x')
    
def devices_update_settings_status_bar(window, device_selected_inst: Device):
    # Update Settings status bar
    main_server_inst = json_dc_inst.get_main_server()
    api_server_inst = json_dc_inst.get_api_server()
    if main_server_inst and last_selected_entity == main_server_inst.NAME:
        window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}, {device_selected_inst}")
    elif api_server_inst and last_selected_entity == api_server_inst.NAME:
        window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}, {device_selected_inst}")
    
def devices_handler(window, event, values):
    global devices_this_device_name 
    global devices_devices_list_box_selection
    global last_selected_entity
    global devices_this_device
    global devices_this_device_ip_str
    
    # inputs
    if event == KEY_DEVICES_NAME_INPUT:
        devices_this_device_name = values[KEY_DEVICES_NAME_INPUT]
    if event == KEY_DEVICES_IP_INPUT:
        devices_this_device_ip_str = values[KEY_DEVICES_IP_INPUT]
    if event == KEY_DEVICES_LIST_BOX_DEVICES:
        devices_devices_list_box_selection = values[KEY_DEVICES_LIST_BOX_DEVICES][0] if values[KEY_DEVICES_LIST_BOX_DEVICES] else None
        devices_this_device = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
        window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update(devices_this_device.get_entities_names())
    if event == KEY_DEVICES_SELECTED_ENTITY_COMBO:
        last_selected_entity = values[KEY_DEVICES_SELECTED_ENTITY_COMBO]
    if event == KEY_DEVICES_ONLINE_LIST_COMBO_BOX:
        window[KEY_DEVICES_IP_INPUT].update(values[KEY_DEVICES_ONLINE_LIST_COMBO_BOX])
        devices_this_device_ip_str = values[KEY_DEVICES_ONLINE_LIST_COMBO_BOX]
    
    if event == KEY_DEVICES_BUTTON_ADD:
        if devices_this_device_name and devices_this_device_name[0].isupper():
            sg.popup_ok(f"Device name must start with lower case letter", title='Adding Device Failed')
        elif devices_this_device_name and devices_this_device_ip_str:
            devices_this_device = Device(devices_this_device_ip_str, devices_this_device_name)
            if not devices_this_device.error():
                if  json_dc_inst.add_device(devices_this_device) == json_dc_inst.DEVICE_ADD_SUCCESS:
                    window[KEY_DEVICES_LIST_BOX_DEVICES].update(json_dc_inst.get_devices_names())
                elif json_dc_inst.add_device(devices_this_device) == json_dc_inst.DEVICE_ADD_ISSUE_WITH_IP:
                    sg.popup_ok("The address is taken, please change the address to another one")
                elif json_dc_inst.add_device(devices_this_device) == json_dc_inst.DEVICE_ADD_ISSUE_WITH_NAME:
                    sg.popup_ok("The device's name is taken, please change the name to another one")       
            else:
                sg.popup_ok("Ip or Name are wrong or exist!")
                
    if event == KEY_DEVICES_BUTTON_LOAD:
        devices_this_device_name = values[KEY_DEVICES_LIST_BOX_DEVICES][0] if values[KEY_DEVICES_LIST_BOX_DEVICES] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if devices_this_device_name:
            devices_this_device = json_dc_inst.get_device_by_name(devices_this_device_name)
            devices_this_device_ip_str = devices_this_device.get_ip().get_address()
            window[KEY_DEVICES_NAME_INPUT].update(devices_this_device_name)
            window[KEY_DEVICES_IP_INPUT].update(devices_this_device_ip_str)
            
    if event == KEY_DEVICES_BUTTON_SAVE:    
        if devices_this_device_name and devices_this_device_name[0].isupper():
            sg.popup_ok(f"Device name must start with lower case letter", title='Adding Device Failed')
        elif devices_this_device and (devices_this_device.get_name() in json_dc_inst.get_devices_names()):
            devices_this_device_name = devices_this_device_name if devices_this_device_name else devices_this_device.get_name()
            devices_this_device_ip_str = devices_this_device_ip_str if devices_this_device_ip_str else devices_this_device.get_ip()
            device_to_remove = devices_this_device
            device_to_remove_name = device_to_remove.get_name()
            entities_names = device_to_remove.get_entities_names()
            devices_without_remove_candidate = json_dc_inst.get_devices_names()
            devices_without_remove_candidate.remove(devices_this_device.get_name())
            if devices_this_device_name not in devices_without_remove_candidate: # if apperas in devices_without_remove_candidate then operation is forbidden
                json_dc_inst.remove_device(device_to_remove_name)
                new_device = Device(devices_this_device_ip_str, devices_this_device_name)
                if not new_device.error():
                    json_dc_inst.add_device(new_device)
                    window[KEY_DEVICES_LIST_BOX_DEVICES].update(json_dc_inst.get_devices_names())
                else:
                    sg.popup_ok("Ip or Name are wrong or exist!")
                # declare on varibales
                main_server_inst = json_dc_inst.get_main_server() if json_dc_inst.get_main_server() else None
                api_server_inst = json_dc_inst.get_api_server() if json_dc_inst.get_api_server() else None
                main_server_name = main_server_inst.get_name() if main_server_inst else None
                api_server_name = api_server_inst.get_name() if api_server_inst else None
                # build device entity list
                for entity_name in entities_names:
                    entity_inst = json_dc_inst.get_entity(entity_name)
                    new_device.add_entity(entity_inst)
                # update special entities status bar
                if main_server_name in new_device.get_entities_names():
                    window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}, {new_device}")   
                if api_server_name in new_device.get_entities_names():
                    window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}, {new_device}")
                window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update(new_device.get_entities_names())
                devices_devices_list_box_selection = "" # prevent update entites box for a device that doesn't exist anymore
            else:
                sg.popup_ok(f"Name {devices_this_device_name} already exists", keep_on_top=True, title="Chance device issue")
                
    if event == KEY_DEVICES_BUTTON_REMOVE and devices_devices_list_box_selection:
        # Update settings status bar
        devices_this_device = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
        main_server_inst = json_dc_inst.get_main_server()
        api_server_inst = json_dc_inst.get_api_server()
        if main_server_inst and main_server_inst.NAME in devices_this_device.get_entities_names():
             window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}, None")
        if api_server_inst and api_server_inst.NAME in devices_this_device.get_entities_names():
             window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}, None")
        
        # Remove device and update GUI      
        json_dc_inst.remove_device(devices_devices_list_box_selection)
        devices_devices_list_box_selection = ""
        last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
        window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update(last_selected_entity, last_entities_list_state_not_occupied)
        window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update("")
        window[KEY_DEVICES_LIST_BOX_DEVICES].update(json_dc_inst.get_devices_names())
        devices_reset_inputs_ui(window)

    if event == KEY_DEVICES_ADD_ENTITY_TO_DEVICE and last_selected_entity and devices_devices_list_box_selection:
        res = json_dc_inst.add_entity_to_device(devices_devices_list_box_selection, last_selected_entity)
        if res == json_dc_inst.ENTITIY_TO_DEVICE_ADD_SUCCESS:
            last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
            window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update("", last_entities_list_state_not_occupied)
            device_selected_inst = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
            window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update(device_selected_inst.get_entities_names())
            devices_update_settings_status_bar(window, device_selected_inst)
        # mange duplicate port 
        elif res == json_dc_inst.ENTITIY_TO_DEVICE_ADD_ISSUE_WITH_PORT: 
            ch = sg.popup_yes_no("The port of the entity you selected is in use, would you like to change it to a random port?",  title="suggest alternative port")
            if ch == "Yes":
                device_selected_inst = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
                entity_selected_inst = json_dc_inst.get_entity(last_selected_entity)
                new_port = device_selected_inst.generate_random_port()
                entity_selected_inst.set_port(new_port) 
                device_selected_inst.add_entity(entity_selected_inst)
                last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
                window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update(last_selected_entity, last_entities_list_state_not_occupied)
                devices_update_settings_status_bar(window, device_selected_inst)
        else:
            sg.popup_ok(f"Could not add {last_selected_entity} to dev: {devices_devices_list_box_selection}")

    if event == KEY_DEVICES_REMOVE_ENTITY_FROM_DEVICE and devices_devices_list_box_selection:
        devices_device_entites_list_box_selection = values[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES][0] if values[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES] else None
        if devices_device_entites_list_box_selection:
            json_dc_inst.remove_entity_from_device(devices_device_entites_list_box_selection)
            # update device selected entitiy combo box 
            last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
            window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update("", last_entities_list_state_not_occupied)
            # update device entities list box 
            device_selected_inst = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
            window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update(device_selected_inst.get_entities_names())
            # update settings_status_bar if needed 
            main_server_inst = json_dc_inst.get_main_server()
            api_server_inst = json_dc_inst.get_api_server()
            if main_server_inst and devices_device_entites_list_box_selection == main_server_inst.NAME:
                window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}")
            if api_server_inst and devices_device_entites_list_box_selection == api_server_inst.NAME:
                window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}")

    #if devices_devices_list_box_selection:
       # devices_this_device = json_dc_inst.get_device_by_name(devices_devices_list_box_selection)
       # window[KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES].update(devices_this_device.get_entities_names())
    

def clients_reset_inputs_ui(window):
    global clients_this_client_name
    global clients_this_client_port
    window[KEY_CLIENTS_NAME_INPUT].update('')
    window[KEY_CLIENTS_PORT_INPUT].update('')
    window[KEY_CLIENTS_STATUS_BAR].update('')

def clients_handler(window, event, values):
    global clients_combo_box_worker_selection
    global clients_this_client_name
    global clients_this_client_port
    global clients_this_client

    if event == KEY_CLIENTS_NAME_INPUT:
        clients_this_client_name = values[KEY_CLIENTS_NAME_INPUT]

    if event == KEY_CLIENTS_PORT_INPUT:
        clients_this_client_port = values[KEY_CLIENTS_PORT_INPUT]

    if event == KEY_CLIENTS_WORKERS_LIST_COMBO_BOX:
        clients_combo_box_worker_selection = values[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX]

    if (event == KEY_CLIENTS_WORKERS_LIST_ADD_WORKER) and clients_combo_box_worker_selection:
        owned_workers_dict = json_dc_inst.get_owned_workers_by_clients_dict()
        if clients_combo_box_worker_selection in owned_workers_dict:  #Todo check this if
            sg.popup_ok(f"worker {clients_combo_box_worker_selection} already belongs to client {owned_workers_dict[clients_combo_box_worker_selection]}", title='Adding a worker failed')
        elif clients_this_client is not None:
            worker_sha = json_dc_inst.get_workers_dict()[clients_combo_box_worker_selection].get_sha()
            clients_this_client.add_worker(clients_combo_box_worker_selection, worker_sha)
            window[KEY_CLIENTS_STATUS_BAR].update(f"Updated client {clients_this_client_name}: {clients_this_client}")
            window[KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS].update(clients_this_client.get_workers_names())
            last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
            window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)
            clients_combo_box_worker_selection = None 
        else:
            sg.popup_ok(f"Add this client before adding workers", keep_on_top=True, title="Add workers issue")

    if event == KEY_CLIENTS_WORKERS_LIST_REMOVE_WORKER:
        clients_this_client_name = values[KEY_ENTITIES_CLIENTS_LISTBOX][0] if values[KEY_ENTITIES_CLIENTS_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        worker_name = values[KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS][0] if values[KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS] else None
        if clients_this_client_name and worker_name:
            clients_this_client = json_dc_inst.get_client(clients_this_client_name)
            clients_this_client.remove_worker(worker_name)
            last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
            window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)
            window[KEY_CLIENTS_STATUS_BAR].update(f"Updated client {clients_this_client_name}: {clients_this_client}")
            window[KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS].update(clients_this_client.get_workers_names())

    if event == KEY_CLIENTS_BUTTON_LOAD:
        clients_this_client_name = values[KEY_ENTITIES_CLIENTS_LISTBOX][0] if values[KEY_ENTITIES_CLIENTS_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if clients_this_client_name:
            clients_this_client = json_dc_inst.get_client(clients_this_client_name)
            clients_this_client_port = clients_this_client.get_port().get_value()
            window[KEY_CLIENTS_NAME_INPUT].update(clients_this_client.get_name())
            window[KEY_CLIENTS_PORT_INPUT].update(f"{clients_this_client_port}")
            window[KEY_CLIENTS_STATUS_BAR].update(f"client {clients_this_client.get_name()} is loaded: {clients_this_client}")
            window[KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS].update(clients_this_client.get_workers_names())
            last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
            window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)

    if event == KEY_CLIENTS_BUTTON_ADD:
        if clients_this_client_name and clients_this_client_name[0].isupper():
            sg.popup_ok(f"Client name must start with lower case letter", title='Adding Client Failed')
        elif clients_this_client_name:       
            clients_this_client = json_dc_inst.get_client(clients_this_client_name)
            if clients_this_client is None and clients_this_client_port: # create a new client
                clients_this_client = Client(clients_this_client_name, clients_this_client_port)
                json_dc_inst.add_client(clients_this_client)
                window[KEY_CLIENTS_STATUS_BAR].update(f"Added client {clients_this_client_name}: {clients_this_client}")
                clients_this_client_name = ''
                clients_this_client_port = ''
                window[KEY_CLIENTS_NAME_INPUT].update(clients_this_client_name) 
                window[KEY_CLIENTS_PORT_INPUT].update(clients_this_client_port) 

    if event == KEY_CLIENTS_BUTTON_SAVE: 
        if clients_this_client_name and clients_this_client_name[0].isupper():
            sg.popup_ok(f"Client name must start with lower case letter", title='Adding Client Failed')   
        elif clients_this_client and (clients_this_client.get_name() in json_dc_inst.get_clients_names()):
            clients_this_client_name = clients_this_client_name if clients_this_client_name else clients_this_client.get_name()
            clients_this_client_port = clients_this_client_port if clients_this_client_port else clients_this_client.get_port()
            client_to_remove = clients_this_client
            client_to_remove_name = clients_this_client.get_name()
            workers_names = client_to_remove.get_workers_names()
            clients_without_remove_candidate = json_dc_inst.get_clients_names()
            clients_without_remove_candidate.remove(clients_this_client.get_name())
            if clients_this_client_name not in clients_without_remove_candidate: # if apperas in clients_without_remove_candidate then operation is forbidden
                json_dc_inst.remove_client(client_to_remove_name)
                new_client = Client(clients_this_client_name, clients_this_client_port)
                json_dc_inst.add_client(new_client)
                for worker_name in workers_names:
                    worker_inst = json_dc_inst.get_worker(worker_name)
                    new_client.add_worker(worker_name, worker_inst.get_sha())
                window[KEY_CLIENTS_STATUS_BAR].update(f"Client {client_to_remove_name} replaced by Client: {new_client}")
            else:
                sg.popup_ok(f"Name {clients_this_client_name} already exists", keep_on_top=True, title="Chance client issue")
                
    if event == KEY_CLIENTS_BUTTON_REMOVE: 
        clients_this_client_name = values[KEY_ENTITIES_CLIENTS_LISTBOX][0] if values[KEY_ENTITIES_CLIENTS_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if clients_this_client_name:
            json_dc_inst.remove_client(clients_this_client_name)
            last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
            window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)
            clients_reset_inputs_ui(window)


def routers_reset_inputs_ui(window):
    global routers_this_router_name
    global routers_this_router_port
    window[KEY_ROUTERS_NAME_INPUT].update('')
    window[KEY_ROUTERS_PORT_INPUT].update('')

def routers_update_inputs_ui(window):
    global routers_this_router_name
    global routers_this_router_port
    global routers_this_router_policy
    window[KEY_ROUTERS_NAME_INPUT].update(routers_this_router_name)
    window[KEY_ROUTERS_PORT_INPUT].update(routers_this_router_port)
    window[KEY_ROUTERS_POLICY_COMBO_BOX].update(value = routers_this_router_policy)

def routers_handler(window,event,values):
    global routers_this_router
    global routers_this_router_name
    global routers_this_router_port
    global routers_this_router_policy

    if event == KEY_ROUTERS_NAME_INPUT:
        routers_this_router_name = values[KEY_ROUTERS_NAME_INPUT]

    if event == KEY_ROUTERS_PORT_INPUT:
        routers_this_router_port = values[KEY_ROUTERS_PORT_INPUT]

    routers_this_router_policy = RouterPolicyDict[values[KEY_ROUTERS_POLICY_COMBO_BOX]] if values[KEY_ROUTERS_POLICY_COMBO_BOX] in RouterPolicyDict else None


    if event == KEY_ROUTERS_BUTTON_ADD and routers_this_router_name and routers_this_router_port and (routers_this_router_policy is not None):
        if routers_this_router_name and routers_this_router_name[0].isupper():
            sg.popup_ok(f"Router name must start with lower case letter", title='Adding Router Failed')
        elif routers_this_router_name:
            routers_this_router = json_dc_inst.get_router(routers_this_router_name)
            if routers_this_router is None:
                # there is no such router - create a new one
                routers_this_router = Router(routers_this_router_name, routers_this_router_port, routers_this_router_policy)
                if not routers_this_router.error():
                    json_dc_inst.add_router(routers_this_router)
                    routers_this_router = None
                    routers_this_router_name = None
                    routers_this_router_port = None
                    routers_this_router_policy = None
                    routers_reset_inputs_ui(window)
            else:
                sg.popup_ok(f"Router {routers_this_router_name} is already exist", title='Adding Client Failed')
    
    if event == KEY_ROUTERS_BUTTON_LOAD:
        routers_this_router_name = values[KEY_ENTITIES_ROUTERS_LISTBOX][0] if values[KEY_ENTITIES_ROUTERS_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if routers_this_router_name:
            routers_this_router = json_dc_inst.get_router(routers_this_router_name)
            routers_this_router_port = routers_this_router.get_port().get_value()
            routers_this_router_policy = routers_this_router.get_policy().get_policy_name()
            routers_update_inputs_ui(window)

    if event == KEY_ROUTERS_BUTTON_REMOVE:
        routers_this_router_name = values[KEY_ENTITIES_ROUTERS_LISTBOX][0] if values[KEY_ENTITIES_ROUTERS_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if routers_this_router_name:
            json_dc_inst.remove_router(routers_this_router_name)
            routers_reset_inputs_ui(window)

def sources_reset_inputs_ui(window):
    global sources_this_source_name
    global sources_this_source_frequency
    global sources_this_source_epochs
    global sources_this_source_port
    global sources_this_source_type
    window[KEY_SOURCES_NAME_INPUT].update('')
    window[KEY_SOURCES_FREQUENCY_INPUT].update('')
    window[KEY_SOURCES_PORT_INPUT].update('')
    window[KEY_SOURCES_EPOCHS_INPUT].update('')
    window[KEY_SOURCES_TYPE_COMBO_BOX].update(value = sources_this_source_type)


def sources_handler(window, event, values):
    global sources_this_source
    global sources_this_source_name
    global sources_this_source_port
    global sources_this_source_frequency
    global sources_this_source_epochs
    global sources_this_source_policy
    global sources_this_source_type

    if (event == KEY_SOURCES_BUTTON_ADD):
        sources_this_source_name = values[KEY_SOURCES_NAME_INPUT]
        if sources_this_source_name and sources_this_source_name[0].isupper():
            sg.popup_ok(f"Source name must start with lower case letter", title='Adding Source Failed')
        elif sources_this_source_name:
            sources_this_source = json_dc_inst.get_source(sources_this_source_name)
            #frequency handling:
            frequency = values[KEY_SOURCES_FREQUENCY_INPUT]
            epochs = Epochs(values[KEY_SOURCES_EPOCHS_INPUT]) if values[KEY_SOURCES_EPOCHS_INPUT] else None
            port = Port(values[KEY_SOURCES_PORT_INPUT]) if values[KEY_SOURCES_PORT_INPUT] else None
            checkbox_val = values[KEY_SOURCES_FREQUENCY_DEFAULT_CHECKBOX]
            if checkbox_val:
                frequency = json_dc_inst.get_frequency()
                if frequency is None:
                    sg.popup_ok(f"Default frequency was selected but never set!", title='Adding Source Failed')
                else:
                    frequency = frequency.get_value_str()
            if bool(sources_this_source_name) and bool(frequency) and (not epochs.error()):
                # new source handling:
                if (sources_this_source is None):
                    sources_this_source_frequency = frequency
                    sources_this_source_epochs = epochs.get_value_str()
                    sources_this_source_port = port.get_value_str()
                    sources_this_source_policy = SourcePolicyDict[values[KEY_SOURCES_POLICY_COMBO_BOX]]
                    sources_this_source_type = SourceTypeDict[values[KEY_SOURCES_TYPE_COMBO_BOX]]
                    sources_this_source = Source(sources_this_source_name, sources_this_source_port, sources_this_source_frequency, sources_this_source_policy, sources_this_source_epochs, sources_this_source_type)
                    json_dc_inst.add_source(sources_this_source)
                    # clear input TextBox
                    sources_this_source = None
                    sources_this_source_name = None
                    sources_this_source_frequency = None
                    sources_this_source_epochs = None
                    sources_this_source_port = None
                    sources_this_source_policy = None
                    sources_this_source_type = SOURCE_TYPE_DICT_DEFAULT_SOURCE_TYPE
                    sources_reset_inputs_ui(window)
                else:
                    sg.popup_ok(f"Source {sources_this_source_name} is already exist", title='Adding Source Failed')
            else:
                sg.popup_ok(f"Missing or wrong fields!", title='Adding Source Failed')

    if event == KEY_SOURCES_BUTTON_LOAD:
        sources_this_source_name = values[KEY_ENTITIES_SOURCES_LISTBOX][0] if values[KEY_ENTITIES_SOURCES_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if sources_this_source_name:
            sources_this_source = json_dc_inst.get_source(sources_this_source_name)
            sources_this_source_frequency = sources_this_source.get_frequency().get_value()
            sources_this_source_epochs = sources_this_source.get_epochs().get_value()
            sources_this_source_port = sources_this_source.get_port().get_value()
            sources_this_source_policy = sources_this_source.get_policy().get_policy_name()
            sources_this_source_type = sources_this_source.get_source_type()
            window[KEY_SOURCES_NAME_INPUT].update(sources_this_source.get_name())
            window[KEY_SOURCES_FREQUENCY_INPUT].update(f"{sources_this_source_frequency}")
            window[KEY_SOURCES_EPOCHS_INPUT].update(f"{sources_this_source_epochs}")
            window[KEY_SOURCES_PORT_INPUT].update(f"{sources_this_source_port}")
            window[KEY_SOURCES_POLICY_COMBO_BOX].update(value = sources_this_source_policy)
            window[KEY_SOURCES_TYPE_COMBO_BOX].update(value = sources_this_source_type)

    if event == KEY_SOURCES_BUTTON_REMOVE:
        sources_this_source_name = values[KEY_ENTITIES_SOURCES_LISTBOX][0] if values[KEY_ENTITIES_SOURCES_LISTBOX] else None  # protects from bypassing load with selection from KEY_DEVICES_SELECTED_ENTITY_COMBO
        if sources_this_source_name:
            json_dc_inst.remove_source(sources_this_source_name)
            sources_this_source_name = ""
            sources_reset_inputs_ui(window)

def entities_handler(window, event, values):
    global entities_clients_names_list
    global entities_routers_names_list
    global entities_sources_names_list
    global last_entities_list_state
    global last_entities_list_state_not_occupied
    global last_selected_entity

    if last_entities_list_state != json_dc_inst.get_entities():
        last_entities_list_state = json_dc_inst.get_entities()
        last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
        window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update("", last_entities_list_state_not_occupied)

    if event == KEY_DEVICES_SELECTED_ENTITY_COMBO:
        last_selected_entity = values[KEY_ENTITIES_CLIENTS_LISTBOX][0] if values[KEY_ENTITIES_CLIENTS_LISTBOX] else None

    if event == KEY_ENTITIES_CLIENTS_LISTBOX:
        last_selected_entity = values[KEY_ENTITIES_CLIENTS_LISTBOX][0] if values[KEY_ENTITIES_CLIENTS_LISTBOX] else None
        window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update(last_selected_entity)

    if event == KEY_ENTITIES_ROUTERS_LISTBOX:
        last_selected_entity = values[KEY_ENTITIES_ROUTERS_LISTBOX][0] if values[KEY_ENTITIES_ROUTERS_LISTBOX] else None
        window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update(last_selected_entity)

    if event == KEY_ENTITIES_SOURCES_LISTBOX:
        last_selected_entity = values[KEY_ENTITIES_SOURCES_LISTBOX][0] if values[KEY_ENTITIES_SOURCES_LISTBOX] else None
        window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update(last_selected_entity)

    # entities update lists
    if json_dc_inst.get_clients_names() != entities_clients_names_list:
        entities_clients_names_list = json_dc_inst.get_clients_names()
        window[KEY_ENTITIES_CLIENTS_LISTBOX].update(entities_clients_names_list)

    if json_dc_inst.get_routers_names() != entities_routers_names_list:
        entities_routers_names_list = json_dc_inst.get_routers_names()
        window[KEY_ENTITIES_ROUTERS_LISTBOX].update(entities_routers_names_list)

    if json_dc_inst.get_sources_names() != entities_sources_names_list:
        entities_sources_names_list = json_dc_inst.get_sources_names()
        window[KEY_ENTITIES_SOURCES_LISTBOX].update(entities_sources_names_list)

def sync_fields_with_json_dc_inst(window, values):
    global json_dc_inst
    global clients_combo_box_worker_selection
    global last_workers_list_state_not_occupied
    global last_workers_list_state
    global last_entities_list_state_not_occupied
    global last_entities_list_state
    global last_selected_entity

    # special entities
    window[KEY_SETTINGS_MAINSERVER_PORT_INPUT].update(json_dc_inst.get_main_server().get_port().get_value_str())
    window[KEY_SETTINGS_MAINSERVER_ARGS_INPUT].update(json_dc_inst.get_main_server().get_args().get_value())
    window[KEY_SETTINGS_APISERVER_PORT_INPUT].update(json_dc_inst.get_api_server().get_port().get_value_str())
    window[KEY_SETTINGS_APISERVER_ARGS_INPUT].update(json_dc_inst.get_main_server().get_args().get_value())
    # settings status bar
    frequency_inst = json_dc_inst.get_frequency()
    batch_size_inst = json_dc_inst.get_batch_size()
    window[KEY_SETTINGS_STATUS_BAR].update(settings_freq_batch_str(frequency_inst.get_value_str(),batch_size_inst.get_value_str()))
    # special entities status bar
    main_server_inst = json_dc_inst.get_main_server()
    api_server_inst = json_dc_inst.get_api_server()
    device_by_main_server = json_dc_inst.get_device_by_entity(main_server_inst.NAME)
    device_by_api_server = json_dc_inst.get_device_by_entity(api_server_inst.NAME)
    window[KEY_SETTINGS_MAIN_SERVER_STATUS_BAR].update(f"Main Server, {main_server_inst}, {device_by_main_server}")   
    window[KEY_SETTINGS_API_SERVER_STATUS_BAR].update(f"Api Server, {api_server_inst}, {device_by_api_server}")
    # settings
    window[KEY_SETTINGS_FREQUENCY_INPUT].update(json_dc_inst.get_frequency().get_value_str()) if json_dc_inst.get_frequency() is not None else None
    window[KEY_SETTINGS_BATCH_SIZE_INPUT].update(json_dc_inst.get_batch_size().get_value_str()) if json_dc_inst.get_batch_size() is not None else None
    # workers
    window[KEY_WORKERS_LIST_BOX].update(json_dc_inst.get_workers_names_list())
    # devices
    window[KEY_DEVICES_LIST_BOX_DEVICES].update(json_dc_inst.get_devices_names())
    # devices - entities
    last_entities_list_state_not_occupied = [x for x in last_entities_list_state if x not in json_dc_inst.get_devices_entities()]
    window[KEY_DEVICES_SELECTED_ENTITY_COMBO].update("", last_entities_list_state_not_occupied)
    # clients - workers
    last_workers_list_state_not_occupied = [x for x in last_workers_list_state if x not in json_dc_inst.get_clients_workers()]
    window[KEY_CLIENTS_WORKERS_LIST_COMBO_BOX].update("", last_workers_list_state_not_occupied)


def dc_json_handler(window, event, values):
    global dc_json_import_file
    global dc_json_export_file
    global json_dc_inst

    if values[KEY_DC_JSON_EXPORT_TO_INPUT_DIR] and \
       values[KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME] and \
       '<' not in values[KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME] and \
       '>' not in values[KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME]:
        dc_json_export_file = values[KEY_DC_JSON_EXPORT_TO_INPUT_DIR] + "/" + values[KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME]

    if event == KEY_DC_JSON_EXPORT_BUTTON and dc_json_export_file and dc_json_export_file.endswith(".json"):
        result = json_dc_inst.export_dc_json(dc_json_export_file)
        if result == json_dc_inst.EXPORT_DC_JSON_ISSUE_MAIN_SERVER_HAS_NO_DEVICE:
            sg.popup_ok(f"MainServer hasn't been associated with device!", title='DC Json Export Issue')
        if result == json_dc_inst.EXPORT_DC_JSON_ISSUE_NO_SPECIAL_ENTITIES_OR_SETTINGS:
            sg.popup_ok(f"DC Json can't be generated due to missing\n special entities or settings!", title='DC Json Export Issue')
        if result == json_dc_inst.EXPORT_DC_JSON_SUCCESS:
            sg.popup_auto_close("Successfully Created", keep_on_top=True)
    
    if event == KEY_DC_JSON_IMPORT_INPUT:
        dc_json_import_file = values[KEY_DC_JSON_IMPORT_INPUT]

    if event == KEY_DC_JSON_IMPORT_BUTTON:
        json_dc_inst_tmp = JsonDistributedConfig()
        res_code, error_str = json_dc_inst_tmp.import_dc_json(dc_json_import_file)
        if (res_code, error_str) != json_dc_inst_tmp.IMPORT_DC_JSON_SUCCESS:
            sg.popup_ok(f"Issue of {error_str}", keep_on_top=True, title="DC json Import Issue")
        else:
            json_dc_inst = json_dc_inst_tmp
            sync_fields_with_json_dc_inst(window, values)

