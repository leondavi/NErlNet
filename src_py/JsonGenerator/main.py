import PySimpleGUI as sg
from Routines import *
from Definitions import *
from WinWorkerDialog import WinWorkerDialog

sg.theme('LightGray4') 

# Json Control 
jsonFields = [  [sg.Text('Load from: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),))],
                [sg.Text('Export to: ')],
                [sg.In(enable_events=True ,key=JSON_CONTROL_EXPORT_BROWSE_EVENT_KEY, expand_x=True), sg.FolderBrowse()],
                [sg.Text('File Name: '), sg.InputText('arc_<name>.json'), sg.Button('Load',  expand_x=True)],
                [sg.Button('Validate', expand_x=True), sg.Button('Create Experiment Flow', expand_x=True)]
            ]
jsonCtrlFrame = sg.Frame("json Control",layout=jsonFields, expand_x=True)


# Specific Fields Frame 
jsonFields = [  [sg.Text('Frequency '), sg.InputText(size=10), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10), sg.Text('# of samples in a message')],
                [sg.Text("Special devices")],
                [sg.Text('Main Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text('API Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text("Optional devices")],
                [sg.Text('NerlGUI'), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Button('Ok',expand_x=True), sg.Button('Cancel', expand_x=True)]]
fieldsFrame = sg.Frame("",layout=jsonFields, expand_x=True)

fieldsAndJsonControlFrames = [[fieldsFrame, jsonCtrlFrame]]

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(40,10))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesFields = [[sg.Button("Add", size=(15)), sg.Button("Load",size=(15))],
                 [sg.Button("Ping",size=(15)), sg.Button("Remove",size=(15))],
                 [sg.Text("name:   "), sg.InputText(size=20)],
                 [sg.Text("ip:        "), sg.InputText(size=20)],
                 [sg.Text("entities:"), sg.InputText(size=(20,5))],
                 [sg.Text("")],
                 [sg.Text("")]]

davicesFieldsFrame = sg.Frame("",devicesFields, expand_x=True)
devicesFrame = sg.Frame("Devices",layout=[[davicesFieldsFrame],[devicesListFrame]], expand_x=True)

# Workers 
workersNamesList = []
workersListFields = [[sg.Text("Workers List")],
                     [sg.Listbox(workersNamesList, size=(40,10) )]]
workersListFrame = sg.Frame("", workersListFields)
workersFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10))],
                 [sg.Button("Validate",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:   "), sg.InputText(size=10)],
                 [sg.Text("path to file of type *.wo/*.xml")],
                 [sg.InputText(size=22), sg.Button("Browse",size=(6))],
                 [sg.Button("Create a new worker .wo",size=(40),enable_events=True,key=WIN_WORKER_DIALOG_EVENT_KEY)],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Entities 
EntitiesNamesList = []
EntitiesListFields = [[sg.Text("Entities List")],[sg.Listbox(EntitiesNamesList, size=(50,20) )]
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
ClientWorkersList = [[sg.Text("workers   "), sg.Multiline(size=(20,10))]]
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
EntitiesFieldsFrame = sg.Frame("", layout=[[ClientsFieldsFrames],[RoutersFieldsFrame],[SourcesFieldsFrame]], expand_x=True)
EntitiesFrame = sg.Frame("Entities - HTTP instances",layout=[[EntitiesFieldsFrame,EntitiesListFrame]], expand_x=True)

# Main Windows
main_window  = sg.Window(title=WINDOW_TITLE, layout=[[sg.Image(NERLNET_LOGO_PATH, expand_x=True)],
                                                     [sg.Text(f'Nerlnet ".json" files generator version {VERSION}')],
                                                     [fieldsAndJsonControlFrames],[workersFrame, devicesFrame],
                                                     [EntitiesFrame]])

while True:
    event, values = main_window.read(timeout=50)
    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break
    if event == JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY:
        update_current_json_file_path(values[JSON_CONTROL_LOAD_FILE_BROWSE_EVENT_KEY])
    if event == WIN_WORKER_DIALOG_EVENT_KEY:
        WinWorkerDialog()
    

main_window.close()