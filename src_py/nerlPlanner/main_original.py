
from Definitions import *
PySimpleGUI_License = PYSIMPLEGUI_5_LICENSE

import PySimpleGUI as sg
from Handlers import *

from WinWorkerDialog import WinWorkerDialog
from WinExperimentFlowDialog import WinExperimentFlowDialog
from WinCommunicationMapDialog import WinCommunicationMapDialog
from JsonElements import *
from Pinger import *
from logger import *
import os
import time
import sys

sg.theme('LightGray4')

print_banner()

# Resolution Care
screen_width, screen_height = get_screen_resolution()
LOG_INFO(f"Screen resolution: {screen_width}x{screen_height}")
if int(screen_width) < WINDOW_FIXED_WIDTH:
    sys.exit(f"ERROR - Minimum resolution width of {WINDOW_FIXED_WIDTH} is required!")

sg.popup_animated(NERLNET_SPLASH_LOGO_PATH) # start splash
DEFAULT_HOST_IP = get_this_host_ip()

# globals
devices_online_hosts_list = [DEFAULT_HOST_IP]

# Settings and Special entities frame
settingsFields = [  
                [sg.Text('Frequency '), sg.InputText(size=10, key=KEY_SETTINGS_FREQUENCY_INPUT, enable_events=True), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10, key=KEY_SETTINGS_BATCH_SIZE_INPUT, enable_events=True), sg.Text('# of samples in a message')],
                [sg.Button("Save", size=(10), key=KEY_SETTINGS_SAVE_BUTTON, enable_events=True),
                 sg.Button("Clear",size=(10), key=KEY_SETTINGS_CLEAR_BUTTON, enable_events=True),
                 sg.Text(settings_freq_batch_str(), key=KEY_SETTINGS_STATUS_BAR, size=(20))]]
settingsFrame = sg.Frame("Settings",layout=settingsFields, expand_x=True, expand_y=True)

specialEntitiesFields = [[sg.Text('Main Server: '), sg.Text('Port'), sg.InputText(size=10, key=KEY_SETTINGS_MAINSERVER_PORT_INPUT, enable_events=True),
                                                   sg.Text('Args'), sg.InputText(size=15, key=KEY_SETTINGS_MAINSERVER_ARGS_INPUT, enable_events=True)],
                        [sg.Text('API Server:  '), 
                                          sg.Text('Port'), sg.InputText(size=10, key=KEY_SETTINGS_APISERVER_PORT_INPUT, enable_events=True),
                                          sg.Text('Args'), sg.InputText(size=15, key=KEY_SETTINGS_APISERVER_ARGS_INPUT, enable_events=True)],
                        [sg.Button("Save", size=(10), key=KEY_SETTINGS_SPECIAL_ENTITIES_SAVE, enable_events=True),
                        sg.Button("Clear",size=(10), key=KEY_SETTINGS_SPECIAL_ENTITIES_CLEAR, enable_events=True)]] 
specialEntitiesFrame = sg.Frame("Special Entities",layout=specialEntitiesFields, expand_x=True, expand_y=True)

jointSettingsAndSpecialEntities = sg.Frame("",layout=[[settingsFrame ,specialEntitiesFrame], 
                                                      [sg.Text(f'This Host IP: {DEFAULT_HOST_IP}', expand_x=True)], 
                                                      [sg.Text('Main Server Status', key=KEY_SETTINGS_MAIN_SERVER_STATUS_BAR, enable_events=True, size=(100), expand_x=True)], 
                                                      [sg.Text('Api Server Status', key=KEY_SETTINGS_API_SERVER_STATUS_BAR, enable_events=True, size=(100), expand_x=True)]])

# Devices 
DevicesNamesList = []
EntitiesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(30,6), enable_events=True, key=KEY_DEVICES_LIST_BOX_DEVICES)]]
devicesEntitiesList = [[sg.Text("Device Entities")],
                     [sg.Listbox(EntitiesNamesList, size=(30,6), enable_events=True, key=KEY_DEVICES_LIST_BOX_DEVICE_ENTITIES)]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesEntitiesListFrame = sg.Frame("", devicesEntitiesList)

devicesFields = [[sg.Button("Scan",size=(10), key=KEY_DEVICES_SCANNER_BUTTON, enable_events=True),sg.Text("lan: "),
                  sg.InputText('x.x.x.x/Mask-bits',size=20, enable_events=True, key=KEY_DEVICES_SCANNER_INPUT_LAN_MASK),
                  sg.Combo(devices_online_hosts_list, size=(15), enable_events=True, key=KEY_DEVICES_ONLINE_LIST_COMBO_BOX)],
                 [sg.Button("Add", size=(10), enable_events=True, key=KEY_DEVICES_BUTTON_ADD),
                  sg.Button("Load",size=(10), enable_events=True, key=KEY_DEVICES_BUTTON_LOAD), 
                  sg.Button("Save",size=(10), enable_events=True, key=KEY_DEVICES_BUTTON_SAVE),
                  sg.Button("Remove",size=(10), enable_events=True, key=KEY_DEVICES_BUTTON_REMOVE)],
                 [sg.Text("Device Name: "), sg.InputText(size=20, enable_events = True, key=KEY_DEVICES_NAME_INPUT)],
                 [sg.Text("IP Address:   "), sg.InputText('x.x.x.x', size=20, enable_events=True, key=KEY_DEVICES_IP_INPUT)],
                 [sg.Text('Selected Device: '), sg.Text('None',key=KEY_DEVICES_SELECTED_DEVICE_TEXT, enable_events=True)],
                 [sg.Text('Selected Entity:', size=(15)), 
                  sg.Combo('entities', size=(15), key=KEY_DEVICES_SELECTED_ENTITY_COMBO, enable_events=True),
                  sg.Button("Add Entity to Selected Device", size=(30), key=KEY_DEVICES_ADD_ENTITY_TO_DEVICE, enable_events=True)],
                 [sg.Text(' ', size=(34)), # fix spaces, more visible 
                  sg.Button("Remove Entity from Selected Device", size=(30), key=KEY_DEVICES_REMOVE_ENTITY_FROM_DEVICE, enable_events=True)]
                  ] #TODO status bar

davicesFieldsFrame = sg.Frame("",devicesFields, expand_x=True)
devicesFrame = sg.Frame("Devices",layout=[[davicesFieldsFrame],[devicesListFrame, devicesEntitiesListFrame]], expand_x=True)

# Workers 
workersNamesList = []
workersListFields = [[sg.Text("Workers List: ",expand_x=True)],
                     [sg.Listbox(workersNamesList, size=(80,6), expand_x=True, key=KEY_WORKERS_LIST_BOX, enable_events=True),
                      sg.Button("Show Worker\nModel", key=KEY_WORKERS_SHOW_WORKER_BUTTON, enable_events=True, expand_x=True, expand_y=True)],
                     [sg.Text("",enable_events=True, key=KEY_WORKERS_INFO_BAR, size=(120))]]
workersListFrame = sg.Frame("", workersListFields)
workersFields = [
                 [sg.Button("Add", size=(10),key=KEY_WORKERS_BUTTON_ADD,enable_events=True),
                  sg.Button("Load", size=8, key=KEY_WORKERS_LOAD_FROM_LIST_WORKER_BUTTON, enable_events=True),
                  sg.Button("View",size=(10),key=KEY_WORKERS_BUTTON_VIEW, enable_events=True), 
                  sg.Button("Remove",size=(10),key=KEY_WORKERS_BUTTON_REMOVE,enable_events=True)],
                 [sg.Text("Name:   "), sg.InputText(size=10,key=KEY_WORKERS_NAME_INPUT, enable_events=True)],
                 [sg.Text("Path to file of type *.json/*.xml")],
                 [sg.InputText(size=60, key=KEY_WORKERS_INPUT_LOAD_WORKER_PATH, enable_events=True), sg.FileBrowse(file_types=(("Worker-File", "*.json"),))],
                 [sg.Button("Create/Edit worker .json",size=(40),enable_events=True,key=WIN_WORKER_DIALOG_EVENT_KEY)],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Clients
ClientsFields = [
                 [sg.Button("Add", size=(9), enable_events=True, key=KEY_CLIENTS_BUTTON_ADD),
                  sg.Button("Load", size=(9), enable_events=True, key=KEY_CLIENTS_BUTTON_LOAD),
                  sg.Button("Save", size=(9), enable_events=True, key=KEY_CLIENTS_BUTTON_SAVE),
                  sg.Button("Remove", size=(9), enable_events=True, key=KEY_CLIENTS_BUTTON_REMOVE)],
                 [sg.Text("Name:  "), sg.InputText(size=15, enable_events=True, key=KEY_CLIENTS_NAME_INPUT)],
                 [sg.Text("Port:    "), sg.InputText(size=15, enable_events=True, key=KEY_CLIENTS_PORT_INPUT)],
                 [sg.Text("Status Bar", key=KEY_CLIENTS_STATUS_BAR, size=(60), enable_events=True, expand_x=True)]
                ]
ClientsFieldsFrame = sg.Frame("",ClientsFields, expand_x=True)

# Client's workers list in multiline
ClientWorkersList = [[sg.Button("Add", size=(10), enable_events=True, key=KEY_CLIENTS_WORKERS_LIST_ADD_WORKER),
                      sg.Button("Remove", size=(10), enable_events=True, key=KEY_CLIENTS_WORKERS_LIST_REMOVE_WORKER),
                      sg.Combo("none", size=(15), enable_events=True, key=KEY_CLIENTS_WORKERS_LIST_COMBO_BOX)],
                     [sg.Text("Workers "), sg.Listbox([],size=(20,6), enable_events=True, key=KEY_CLIENTS_WORKERS_LIST_BOX_CLIENT_FOCUS)]]
ClientWorkersFrame = sg.Frame("",ClientWorkersList)

ClientsFieldsFrames = sg.Frame("Clients",layout=[[ClientsFieldsFrame,ClientWorkersFrame]],expand_x=True)

# Routers
RoutersFields = [
                 [sg.Button("Add", size=(9), key=KEY_ROUTERS_BUTTON_ADD, enable_events=True),
                  sg.Button("Load",size=(9), key=KEY_ROUTERS_BUTTON_LOAD, enable_events=True),
                   sg.Button("Save",size=(9), key=KEY_ROUTERS_BUTTON_SAVE, enable_events=True),
                  sg.Button("Remove",size=(9), key=KEY_ROUTERS_BUTTON_REMOVE, enable_events=True)],
                 [sg.Text("Name:  "), sg.InputText(size=15, enable_events=True, key=KEY_ROUTERS_NAME_INPUT), 
                  sg.Text("Policy: "), sg.Combo(list(RouterPolicyDict.keys()),size=15, enable_events=True, key=KEY_ROUTERS_POLICY_COMBO_BOX)],
                 [sg.Text("Port:    "), sg.InputText(size=15, enable_events=True, key=KEY_ROUTERS_PORT_INPUT)],
                ]
RoutersFieldsFrame = sg.Frame("Routers",RoutersFields)

# Sources
SourcesFields = [
                 [sg.Button("Add", size=(9), key=KEY_SOURCES_BUTTON_ADD, enable_events=True),
                  sg.Button("Load",size=(9), key=KEY_SOURCES_BUTTON_LOAD, enable_events=True),
                  sg.Button("Save",size=(9), key=KEY_SOURCES_BUTTON_SAVE, enable_events=True),
                  sg.Button("Remove",size=(9), key=KEY_SOURCES_BUTTON_REMOVE, enable_events=True)],
                 [sg.Text("Name:  "), sg.InputText(size=15, key=KEY_SOURCES_NAME_INPUT, enable_events=True), 
                  sg.Text("Frequency:  "), sg.InputText(size=10, key=KEY_SOURCES_FREQUENCY_INPUT, enable_events=True),
                  sg.Checkbox("Default", key=KEY_SOURCES_FREQUENCY_DEFAULT_CHECKBOX, enable_events=True)],
                 [sg.Text("Port:     "), sg.InputText(size=15, key=KEY_SOURCES_PORT_INPUT, enable_events=True),
                  sg.Text("Epochs:      "), sg.InputText(size=10, key=KEY_SOURCES_EPOCHS_INPUT, enable_events=True) ],
                  [sg.Text("Policy:  "), sg.Combo(list(SourcePolicyDict.keys()), size=15, key=KEY_SOURCES_POLICY_COMBO_BOX, enable_events=True),
                   sg.Text("Type:    "), sg.Combo(list(SourceTypeDict.keys()), default_value=SOURCE_TYPE_DICT_DEFAULT_SOURCE_TYPE, size=15, key=KEY_SOURCES_TYPE_COMBO_BOX, enable_events=True)]
                ]
SourcesFieldsFrame = sg.Frame("Sources",SourcesFields)

# Entities Frame
EntitiesNamesList = []
EntitiesListFields = [[sg.Text("Clients", expand_x=True), sg.Text("Routers", expand_x=True), sg.Text("Sources", expand_x=True)],
                      [sg.Listbox(EntitiesNamesList, enable_events=True, key=KEY_ENTITIES_CLIENTS_LISTBOX, size=(20,14)),
                       sg.Listbox(EntitiesNamesList, enable_events=True, key=KEY_ENTITIES_ROUTERS_LISTBOX, size=(20,14)),
                       sg.Listbox(EntitiesNamesList, enable_events=True, key=KEY_ENTITIES_SOURCES_LISTBOX, size=(20,14))],
                      ]
EntitiesFieldsFrame = sg.Frame("", layout=[[ClientsFieldsFrames],[SourcesFieldsFrame, RoutersFieldsFrame]])
EntitiesListFrame = sg.Frame("", EntitiesListFields)
EntitiesFrame = sg.Frame("Entities - HTTP Cowboy instances",layout=[[EntitiesFieldsFrame, EntitiesListFrame]], expand_x=True)



# Distributed Configuration (DC) Json File Section
JsonFileFields = [  [sg.Text('Import DC Json File: ')],
                [sg.In(enable_events=True ,key=KEY_DC_JSON_IMPORT_INPUT, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),))],
                [sg.Text('Export To DC Json File:')],
                [sg.Text('Directory:'),sg.In(enable_events=True ,key=KEY_DC_JSON_EXPORT_TO_INPUT_DIR), sg.FolderBrowse()],
                [sg.Text('File Name:'), sg.InputText('dc_<name>.json', enable_events=True, key=KEY_DC_JSON_EXPORT_TO_INPUT_FILENAME)],
                [sg.Button('Export',expand_x=True, enable_events=True, key=KEY_DC_JSON_EXPORT_BUTTON),
                 sg.Button('Import', expand_x=True, enable_events=True, key=KEY_DC_JSON_IMPORT_BUTTON),
                 sg.Button('Clear', expand_x=True, enable_events=True, key=KEY_DC_JSON_CLEAR_BUTTON)]
                ]
jsonCtrlFrame = sg.Frame("Distributed Configurations Json",layout=JsonFileFields, expand_x=True)

# Graph and Experimant generate buttons (open a new window for these jsons)
grapAndExpFields = [[sg.Button('Generate\nCommunication Map', expand_x=True, expand_y=True, key=WIN_COMMUNICATION_MAP_DIALOG_EVENT_KEY)],
                    [sg.Button('Generate\nExperiment Flow', expand_x=True, expand_y=True, key=WIN_EXPERIMENT_FLOW_DIALOG_EVENT_KEY)]]
grapAndExpFrame = sg.Frame("Graph and Experiment",layout=grapAndExpFields, expand_x=True, expand_y=True)

jsonCtrlFrameWithSettingsAndSpecialEntities = sg.Frame("",layout = [[jointSettingsAndSpecialEntities, jsonCtrlFrame, grapAndExpFrame]], expand_x = True)

overall_layout = [[sg.Image(NERLNET_LOGO_PATH, expand_x=True)],
                                                     [sg.Text(f'Nerlnet Planner v-{VERSION}')],
                                                    [jsonCtrlFrameWithSettingsAndSpecialEntities],
                                                    [EntitiesFrame],
                                                    [workersFrame, devicesFrame ]
                                                    ]

scrollable_enable = False if WINDOW_MAX_SUPPORTED_HEIGHT < int(screen_height) else True
height_size = min(int(int(screen_height) * WINDOW_HEIGHT_MULTIPLICATION_FACTOR), WINDOW_MAX_SUPPORTED_HEIGHT)
overall_column = [[sg.Column(layout=overall_layout, scrollable=scrollable_enable, size = (WINDOW_FIXED_WIDTH,height_size), vertical_scroll_only=True, expand_x=True)]]

# Main Windows
main_window  = sg.Window(title=WINDOW_TITLE, layout=overall_column, size = (WINDOW_FIXED_WIDTH,height_size))

os.makedirs(os.path.dirname(NERLNET_TMP_PATH), exist_ok=True)

time.sleep(2)
sg.popup_animated(None) # end splash

while True:
    event, values = main_window.read(timeout=50)
    
    if event and values:
        settings_handler(main_window, event,values)
        workers_handler(main_window, event,values)
        clients_handler(main_window, event, values)
        routers_handler(main_window, event, values)
        sources_handler(main_window, event, values)
        entities_handler(main_window, event, values)
        devices_handler(main_window, event, values)
        online_scanner_handler(main_window, event, values, devices_online_hosts_list) # lan scan for online devices
        dc_json_handler(main_window, event, values)

    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break

    if event == WIN_WORKER_DIALOG_EVENT_KEY:
        WinWorkerDialog()
    
    if event == WIN_EXPERIMENT_FLOW_DIALOG_EVENT_KEY:
        WinExperimentFlowDialog()

    if event == WIN_COMMUNICATION_MAP_DIALOG_EVENT_KEY:
        WinCommunicationMapDialog()

main_window.close()