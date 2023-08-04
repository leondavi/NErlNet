import PySimpleGUI as sg
from Handlers import *
from Definitions import *
from WinWorkerDialog import WinWorkerDialog
from JsonElements import *
from Pinger import *
import logging
import os
import time

sg.theme('LightGray4')

print_banner()

sg.popup_animated(NERLNET_SPLASH_LOGO_PATH) # start splash
DEFAULT_HOST_IP = get_this_host_ip()

# globals
devices_online_hosts_list = [DEFAULT_HOST_IP]

# Specific Fields Frame 
settingsFields = [  
                [sg.Text('Frequency '), sg.InputText(size=10, key=KEY_SETTINGS_FREQUENCY_INPUT, enable_events=True), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10, key=KEY_SETTINGS_BATCH_SIZE_INPUT, enable_events=True), sg.Text('# of samples in a message')],
                [sg.Text("Special devices")],
                [sg.Text('Main Server: '), sg.Text('IP'), sg.InputText(DEFAULT_HOST_IP, size=15, key=KEY_SETTINGS_MAINSERVER_IP_INPUT, enable_events=True), 
                                           sg.Text('Port'), sg.InputText(size=10, key=KEY_SETTINGS_MAINSERVER_PORT_INPUT, enable_events=True),
                                           sg.Text('Args'), sg.InputText(size=15, key=KEY_SETTINGS_MAINSERVER_ARGS_INPUT, enable_events=True)],
                [sg.Text('API Server:  '), sg.Text('IP'), sg.InputText(DEFAULT_HOST_IP, size=15, key=KEY_SETTINGS_APISERVER_IP_INPUT, enable_events=True), 
                                          sg.Text('Port'), sg.InputText(size=10, key=KEY_SETTINGS_APISERVER_PORT_INPUT, enable_events=True),
                                          sg.Text('Args'), sg.InputText(size=15, key=KEY_SETTINGS_APISERVER_ARGS_INPUT, enable_events=True)],
                                          [sg.Text('Status Bar')], # TODO complete status bar activity
                [sg.Button("Save", size=(10), key=KEY_SETTINGS_SAVE_BUTTON, enable_events=True), sg.Button("Clear",size=(10))],
            ]
settingsFrame = sg.Frame("Settings",layout=settingsFields, expand_x=True, expand_y=True)

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(30,6))]]
devicesEntitiesList = [[sg.Text("Device Entities")],
                     [sg.Listbox(DevicesNamesList, size=(30,6))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesEntitiesListFrame = sg.Frame("", devicesEntitiesList)

devicesFields = [[sg.Button("Scan",size=(10), key=KEY_DEVICES_SCANNER_BUTTON, enable_events=True),sg.Text("lan: "),
                  sg.InputText('x.x.x.x/Mask-bits',size=20, enable_events=True, key=KEY_DEVICES_SCANNER_INPUT_LAN_MASK),
                  sg.Combo(devices_online_hosts_list, size=(15), enable_events=True, key=KEY_DEVICES_ONLINE_LIST_COMBO_BOX)],
                 [sg.Button("Add", size=(10)), sg.Button("Update",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("Device Name: "), sg.InputText(size=20, enable_events = True, key=KEY_DEVICES_NAME_INPUT)],
                 [sg.Text("IP Address:   "), sg.InputText('x.x.x.x', size=20, enable_events=True, key=KEY_DEVICES_IP_INPUT)],
                 [sg.Text('Selected Device: '), sg.Text('None',key=KEY_DEVICES_SELECTED_DEVICE_TEXT, enable_events=True)],
                 [sg.Text('Selected Entity:', size=(15)), 
                  sg.Combo('entities', size=(15), key=KEY_DEVICES_SELECTED_ENTITY_COMBO, enable_events=True), sg.Button("Add Entity to Selected Device", size=(25))]
                  ] #TODO status bar

davicesFieldsFrame = sg.Frame("",devicesFields, expand_x=True)
devicesFrame = sg.Frame("Devices",layout=[[davicesFieldsFrame],[devicesListFrame, devicesEntitiesListFrame]], expand_x=True)

# Workers 
workersNamesList = []
workersListFields = [[sg.Text("Workers List")],
                     [sg.Listbox(workersNamesList, size=(90,6), key=KEY_WORKERS_LIST_BOX, enable_events=True), sg.Button("Load", size=8, key=KEY_WORKERS_LOAD_FROM_LIST_WORKER_BUTTON, enable_events=True)],
                     [sg.Text("",enable_events=True, key=KEY_WORKERS_INFO_BAR)]]
workersListFrame = sg.Frame("", workersListFields)
workersFields = [
                 [sg.Button("Add", size=(10),key=KEY_WORKERS_BUTTON_ADD,enable_events=True),
                  sg.Button("View",size=(10),key=KEY_WORKERS_BUTTON_VIEW, enable_events=True), 
                  sg.Button("Remove",size=(10),key=KEY_WORKERS_BUTTON_REMOVE,enable_events=True)],
                 [sg.Text("Name:   "), sg.InputText(size=10,key=KEY_WORKERS_NAME_INPUT, enable_events=True)],
                 [sg.Text("Path to file of type *.json/*.xml")],
                 [sg.InputText(size=60, key=KEY_WORKERS_INPUT_LOAD_WORKER_PATH, enable_events=True), sg.FileBrowse(file_types=(("Worker-File", "*.json"),)),
                  sg.Button("Show", key=KEY_WORKERS_SHOW_WORKER_BUTTON, enable_events=True)],
                 [sg.Button("Create/Edit worker .json",size=(40),enable_events=True,key=WIN_WORKER_DIALOG_EVENT_KEY)],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Clients
ClientsFields = [
                 [sg.Button("Add", size=(10), enable_events=True, key=KEY_CLIENTS_BUTTON_ADD),
                  sg.Button("Load", size=(10), enable_events=True, key=KEY_CLIENTS_BUTTON_LOAD),
                  sg.Button("Remove", size=(10), enable_events=True, key=KEY_CLIENTS_BUTTON_REMOVE)],
                 [sg.Text("Name:  "), sg.InputText(size=15, enable_events=True, key=KEY_CLIENTS_NAME_INPUT)],
                 [sg.Text("Port:    "), sg.InputText(size=15, enable_events=True, key=KEY_CLIENTS_PORT_INPUT)],
                 [sg.Text("Status Bar", key=KEY_CLIENTS_STATUS_BAR, enable_events=True, expand_x=True)]
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
                 [sg.Button("Add", size=(10), key=KEY_ROUTERS_BUTTON_ADD, enable_events=True),
                  sg.Button("Load",size=(10), key=KEY_ROUTERS_BUTTON_LOAD, enable_events=True),
                  sg.Button("Remove",size=(10), key=KEY_CLIENTS_BUTTON_REMOVE, enable_events=True)],
                 [sg.Text("Name:  "), sg.InputText(size=15, enable_events=True, key=KEY_ROUTERS_NAME_INPUT), 
                  sg.Text("Policy: "), sg.Combo(list(RouterPolicyDict.keys()),size=15, enable_events=True, key=KEY_ROUTERS_POLICY_COMBO_BOX)],
                 [sg.Text("Port:    "), sg.InputText(size=15, enable_events=True, key=KEY_ROUTERS_PORT_INPUT)],
                ]
RoutersFieldsFrame = sg.Frame("Routers",RoutersFields)

# Sources
SourcesFields = [
                 [sg.Button("Add", size=(10), key=KEY_SOURCES_BUTTON_ADD, enable_events=True),
                  sg.Button("Load",size=(10), key=KEY_SOURCES_BUTTON_LOAD, enable_events=True),
                  sg.Button("Remove",size=(10), key=KEY_SOURCES_BUTTON_REMOVE, enable_events=True)],
                 [sg.Text("Name:  "), sg.InputText(size=15, key=KEY_SOURCES_NAME_INPUT, enable_events=True), 
                  sg.Text("Frequency:  "), sg.InputText(size=10, key=KEY_SOURCES_FREQUENCY_INPUT, enable_events=True),
                  sg.Checkbox("Default", key=KEY_SOURCES_FREQUENCY_DEFAULT_CHECKBOX, enable_events=True)],
                 [sg.Text("Port:     "), sg.InputText(size=15, key=KEY_SOURCES_PORT_INPUT, enable_events=True),
                  sg.Text("Epochs:      "), sg.InputText(size=10, key=KEY_SOURCES_EPOCHS_INPUT, enable_events=True) ],
                  [sg.Text("Policy:  "), sg.Combo(list(SourcePolicyDict.keys()), size=15, key=KEY_SOURCES_POLICY_COMBO_BOX, enable_events=True) ]
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
EntitiesFrame = sg.Frame("Entities - HTTP Cowboy instances",layout=[[EntitiesFieldsFrame, EntitiesListFrame]])





# Json File Control
JsonFileFields = [  [sg.Text('Load from: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),))],
                [sg.Text('Export to: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_EXPORT_BROWSE_EVENT_KEY, expand_x=True), sg.FolderBrowse()],
                [sg.Text('File Name: '), sg.InputText('dc_<name>.json'), sg.Button('Load',  expand_x=True)],
                [sg.Button('Export',expand_x=True), sg.Button('Validate', expand_x=True), sg.Button('Load', expand_x=True), sg.Button('Clear', expand_x=True)]
                ]
jsonCtrlFrame = sg.Frame("Distributed Configurations Json",layout=JsonFileFields, expand_x=True)


# Graph and Experimant generate buttons (open a new window for these jsons)
grapAndExpFields = [[sg.Button('Generate Graph', expand_x=True), sg.Button('Generate Experiment', expand_x=True)]]
grapAndExpFrame = sg.Frame("Graph and Experiment",layout=grapAndExpFields, expand_x=True)


# Main Windows
main_window  = sg.Window(title=WINDOW_TITLE, layout=[[sg.Image(NERLNET_LOGO_PATH, expand_x=True)],
                                                     [sg.Text(f'Nerlnet Planner v-{VERSION}')],
                                                    [settingsFrame, jsonCtrlFrame],
                                                    [EntitiesFrame],
                                                    [workersFrame, devicesFrame ],
                                                    [grapAndExpFrame]
                                                    ])

os.makedirs(os.path.dirname(NERLNET_TMP_PATH), exist_ok=True)

time.sleep(2)
sg.popup_animated(None) # end splash

while True:
    event, values = main_window.read(timeout=50)
    
    if event and values:
        settings_handler(event,values)
        workers_handler(main_window, event,values)
        clients_handler(main_window, event, values)
        routers_handler(main_window, event, values)
        sources_handler(main_window, event, values)
        entities_handler(main_window, event, values)
        devices_handler(main_window, event, values)
        online_scanner_handler(main_window, event, values, devices_online_hosts_list) # lan scan for online devices

    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break

    if event == JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY:
        update_current_json_file_path(values[JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY])
    if event == WIN_WORKER_DIALOG_EVENT_KEY:
        WinWorkerDialog()
    

main_window.close()