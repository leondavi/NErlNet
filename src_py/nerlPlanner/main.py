import PySimpleGUI as sg
from Handlers import *
from Definitions import *
from WinWorkerDialog import WinWorkerDialog
from JsonElements import *
import logging

sg.theme('LightGray4') 

# Specific Fields Frame 
settingsFields = [  [sg.Text('Frequency '), sg.InputText(size=10, key=KEY_SETTINGS_FREQUENCY_INPUT, enable_events=True), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10, key=KEY_SETTINGS_BATCH_SIZE_INPUT, enable_events=True), sg.Text('# of samples in a message')],
                [sg.Text("Special devices")],
                [sg.Text('Main Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text('API Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text('NerlGUI (optional)'), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Button("Add", size=(10), key=KEY_SETTINGS_ADD_BUTTON, enable_events=True), sg.Button("Clear",size=(10))],
            ]
settingsFrame = sg.Frame("Settings",layout=settingsFields, expand_x=True)

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(30,8))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesFields = [[sg.Button("Add", size=(15)), sg.Button("Load",size=(15))],
                 [sg.Button("Ping",size=(15)), sg.Button("Remove",size=(15))],
                 [sg.Text("name:   "), sg.InputText(size=20)],
                 [sg.Text("ip:        "), sg.InputText(size=20)],
                 [sg.Text("entities:"), sg.InputText(size=(20,5))]]

davicesFieldsFrame = sg.Frame("",devicesFields, expand_x=True)
devicesFrame = sg.Frame("Devices",layout=[[davicesFieldsFrame],[devicesListFrame]], expand_x=True)

# Workers 
workersNamesList = []
workersListFields = [[sg.Text("Workers List")],
                     [sg.Listbox(workersNamesList, size=(30,6) )]]
workersListFrame = sg.Frame("", workersListFields)
workersFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Validate",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:   "), sg.InputText(size=10)],
                 [sg.Text("path to file of type *.json/*.xml")],
                 [sg.InputText(size=22), sg.Button("Browse",size=(6))],
                 [sg.Button("Create/Edit worker .json",size=(40),enable_events=True,key=WIN_WORKER_DIALOG_EVENT_KEY)],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Entities 
EntitiesNamesList = []
EntitiesListFields = [[sg.Text("Entities List")],[sg.Listbox(EntitiesNamesList, size=(25,10) )]
                                     ]
EntitiesListFrame = sg.Frame("", EntitiesListFields)

# Clients
ClientsFields = [
                 [sg.Button("Add"), sg.Button("Load")],
                 [sg.Text("name:  "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)],
                 [sg.Button("Remove")]
                ]
ClientsFieldsFrame = sg.Frame("",ClientsFields)

# Client's workers list in multiline
ClientWorkersList = [[sg.Text("workers   "), sg.Multiline(size=(20,6))]]
ClientWorkersFrame = sg.Frame("",ClientWorkersList)

ClientsFieldsFrames = sg.Frame("Clients",layout=[[ClientsFieldsFrame,ClientWorkersFrame]], expand_x=True)

# Routers
RoutersFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:  "), sg.InputText(size=15), sg.Text("policy   "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)],
                ]
RoutersFieldsFrame = sg.Frame("Routers",RoutersFields, expand_x=True)

# Sources
SourcesFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:  "), sg.InputText(size=15), sg.Text("frequency:  "), sg.InputText(size=15)],
                 [sg.Text("port:    "), sg.InputText(size=15)],
                ]
SourcesFieldsFrame = sg.Frame("Sources",SourcesFields, expand_x=True)

# Entities Frame
EntitiesFieldsFrame = sg.Frame("", layout=[[ClientsFieldsFrames, RoutersFieldsFrame],[SourcesFieldsFrame]], expand_x=True)
EntitiesFrame = sg.Frame("Entities - HTTP instances",layout=[[EntitiesFieldsFrame]], expand_x=True)


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
                                                     [sg.Text(f'Nerlnet ".json" files generator version {VERSION}')],
                                                    [settingsFrame,EntitiesListFrame],
                                                    [devicesFrame, workersFrame],
                                                    [EntitiesFrame],
                                                    [jsonCtrlFrame]])



while True:
    event, values = main_window.read(timeout=50)
    settings_handler(event,values)
    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break
    if event == JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY:
        update_current_json_file_path(values[JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY])
    if event == WIN_WORKER_DIALOG_EVENT_KEY:
        WinWorkerDialog()
    

main_window.close()