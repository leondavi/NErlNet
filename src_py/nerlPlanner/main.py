import PySimpleGUI as sg
from Handlers import *
from Definitions import *
from WinWorkerDialog import WinWorkerDialog
from JsonElements import *
from Pinger import *
import logging

sg.theme('LightGray4') 

# globals
devices_online_hosts_list = ['127.0.0.1']

# Specific Fields Frame 
settingsFields = [  [sg.Text('Frequency '), sg.InputText(size=10, key=KEY_SETTINGS_FREQUENCY_INPUT, enable_events=True), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10, key=KEY_SETTINGS_BATCH_SIZE_INPUT, enable_events=True), sg.Text('# of samples in a message')],
                [sg.Text("Special devices")],
                [sg.Text('Main Server: '), sg.Text('ip'), sg.InputText(size=15, key=KEY_SETTINGS_MAINSERVER_IP_INPUT, enable_events=True), 
                                           sg.Text('port'), sg.InputText(size=10, key=KEY_SETTINGS_MAINSERVER_PORT_INPUT, enable_events=True),
                                           sg.Text('args'), sg.InputText(size=15, key=KEY_SETTINGS_MAINSERVER_ARGS_INPUT, enable_events=True)],
                [sg.Text('API Server:  '), sg.Text('ip'), sg.InputText(size=15, key=KEY_SETTINGS_APISERVER_IP_INPUT, enable_events=True), 
                                          sg.Text('port'), sg.InputText(size=10, key=KEY_SETTINGS_APISERVER_PORT_INPUT, enable_events=True),
                                          sg.Text('args'), sg.InputText(size=15, key=KEY_SETTINGS_APISERVER_ARGS_INPUT, enable_events=True)],
                [sg.Text('NerlGUI:      '), sg.Text('ip'), sg.InputText(size=15, key=KEY_SETTINGS_NERLGUI_IP_INPUT, enable_events=True),
                                     sg.Text('port'), sg.InputText(size=10, key=KEY_SETTINGS_NERLGUI_PORT_INPUT, enable_events=True),
                                     sg.Text('args'), sg.InputText(size=15, key=KEY_SETTINGS_NERLGUI_ARGS_INPUT, enable_events=True),sg.Checkbox("enable nerlGUI", default=False, key=KEY_CHECKBOX_ENABLE_NERLGUI, enable_events=True)],
                [sg.Button("Add", size=(10), key=KEY_SETTINGS_ADD_BUTTON, enable_events=True), sg.Button("Clear",size=(10))],
            ]
settingsFrame = sg.Frame("Settings",layout=settingsFields, expand_x=True)

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(30,6))]]
devicesEntitiesList = [[sg.Text("Device Entities")],
                     [sg.Listbox(DevicesNamesList, size=(30,6))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesEntitiesListFrame = sg.Frame("", devicesEntitiesList)

devicesFields = [[sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Button("Scan",size=(10), key=KEY_DEVICES_SCANNER_BUTTON, enable_events=True),sg.Text("lan: "),
                  sg.InputText('x.x.x.x/mask',size=20, enable_events=True, key=KEY_DEVICES_SCANNER_INPUT_LAN_MASK),
                  sg.Combo(devices_online_hosts_list,enable_events=True, key=KEY_DEVICES_ONLINE_LIST_COMBO_BOX)],
                 [sg.Text("device name: "), sg.InputText(size=20, enable_events = True, key=KEY_DEVICES_NAME_INPUT)],
                 [sg.Text("ip address:   "), sg.InputText('x.x.x.x', size=20, enable_events=True, key=KEY_DEVICES_IP_INPUT)],
                 [sg.InputText('selected entity', size=(15)), sg.Combo('entities', size=(15)), sg.Button("add", size=(15))]]

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
                 [sg.Text("name:   "), sg.InputText(size=10,key=KEY_WORKERS_NAME_INPUT, enable_events=True)],
                 [sg.Text("path to file of type *.json/*.xml")],
                 [sg.InputText(size=60, key=KEY_WORKERS_INPUT_LOAD_WORKER_PATH, enable_events=True), sg.FileBrowse(file_types=(("Worker-File", "*.json"),)),
                  sg.Button("Show", key=KEY_WORKERS_SHOW_WORKER_BUTTON, enable_events=True)],
                 [sg.Button("Create/Edit worker .json",size=(40),enable_events=True,key=WIN_WORKER_DIALOG_EVENT_KEY)],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Clients
ClientsFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load", size=(10)), sg.Button("Remove", size=(10)) ],
                 [sg.Text("name:  "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)]
                ]
ClientsFieldsFrame = sg.Frame("",ClientsFields, expand_x=True)

# Client's workers list in multiline
ClientWorkersList = [[sg.Button("Add", size=(10)), sg.Button("Remove", size=(10)), sg.Combo("workers", size=(15), enable_events=True, key=KEY_CLIENTS_WORKERS_LIST_COMBO_BOX)],
                     [sg.Text("workers "), sg.Listbox([],size=(20,6))]]
ClientWorkersFrame = sg.Frame("",ClientWorkersList)

ClientsFieldsFrames = sg.Frame("Clients",layout=[[ClientsFieldsFrame,ClientWorkersFrame]])

# Routers
RoutersFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:  "), sg.InputText(size=15), sg.Text("policy   "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)],
                ]
RoutersFieldsFrame = sg.Frame("Routers",RoutersFields)

# Sources
SourcesFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:  "), sg.InputText(size=15), sg.Text("frequency:  "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)],
                ]
SourcesFieldsFrame = sg.Frame("Sources",SourcesFields)

# Entities Frame
EntitiesNamesList = []
EntitiesListFields = [[sg.Text("Clients", expand_x=True), sg.Text("Routers", expand_x=True), sg.Text("Sources", expand_x=True)],
                      [sg.Listbox(EntitiesNamesList, size=(20,14) ), sg.Listbox(EntitiesNamesList, size=(20,14) ), sg.Listbox(EntitiesNamesList, size=(20,14) )]
                                     ]
EntitiesFieldsFrame = sg.Frame("", layout=[[ClientsFieldsFrames],[SourcesFieldsFrame, RoutersFieldsFrame]])
EntitiesListFrame = sg.Frame("", EntitiesListFields)
EntitiesFrame = sg.Frame("Entities - HTTP Cowboy instances",layout=[[EntitiesFieldsFrame, EntitiesListFrame]])





# Json File Control
JsonFileFields = [  [sg.Text('Load from: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),))],
                [sg.Text('Export to: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_EXPORT_BROWSE_EVENT_KEY, expand_x=True), sg.FolderBrowse()],
                [sg.Text('File Name: '), sg.InputText('arc_<name>.json'), sg.Button('Load',  expand_x=True)],
                [sg.Button('Export',expand_x=True), sg.Button('Validate', expand_x=True), sg.Button('Load', expand_x=True), sg.Button('Clear', expand_x=True)],
                [sg.Button('Create Experiment Flow', expand_x=True)] ]
jsonCtrlFrame = sg.Frame("json Control",layout=JsonFileFields, expand_x=True)

# Main Windows
main_window  = sg.Window(title=WINDOW_TITLE, layout=[[sg.Image(NERLNET_LOGO_PATH, expand_x=True)],
                                                     [sg.Text(f'Nerlnet Planner v-{VERSION}')],
                                                    [settingsFrame, jsonCtrlFrame],
                                                    [EntitiesFrame],
                                                    [workersFrame, devicesFrame ]
                                                    ])

while True:
    event, values = main_window.read(timeout=50)
    settings_handler(event,values)
    clients_handler(main_window, event, values)
    workers_handler(main_window,event,values)
    online_scanner_handler(main_window, event, values, devices_online_hosts_list) # lan scan for online devices

    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break

    if event == JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY:
        update_current_json_file_path(values[JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY])
    if event == WIN_WORKER_DIALOG_EVENT_KEY:
        WinWorkerDialog()
    

main_window.close()