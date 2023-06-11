import PySimpleGUI as sg

VERSION = "0.0.1"
NERLNET_GLBOAL_PATH = "/usr/local/lib/nerlnet-lib/NErlNet"
NERLNET_LOGO_PATH = NERLNET_GLBOAL_PATH+"/src_py/JsonGenerator/NerlnetIcon.png"
WINDOW_TITLE = "Nerlnet Json Architecture Generator"

sg.theme('LightGray4') 



# Specific Fields Frame 
jsonFields = [  [sg.Text('Frequency '), sg.InputText(size=10), sg.Text('Default frequency for sensors')],
                [sg.Text('Batch Size'), sg.InputText(size=10), sg.Text('Number of samples sending in a message')],
                [sg.Text("Special devices")],
                [sg.Text('Main Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text('API Server: '), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Text("Optional devices")],
                [sg.Text('NerlGUI'), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Button('Ok',expand_x=True), sg.Button('Cancel', expand_x=True)]]
fieldsFrame = sg.Frame("",layout=jsonFields, expand_x=True)

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(40,10))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesFields = [[sg.Button("Add", size=(10)), sg.Button("Load",size=(10))],
                 [sg.Button("Ping",size=(10)), sg.Button("Remove",size=(10))],
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
                 [sg.Button("Create a new worker .wo",size=(40))],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)


# Entities 
EntitiesValues = ['client','router','source']
EntitiesNamesList = []
EntitiesListFields = [[sg.Text("Entities List")],
                     [sg.Listbox(EntitiesNamesList, size=(100,5) )]]
EntitiesListFrame = sg.Frame("", EntitiesListFields)
EntitiesFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("type:   "), sg.OptionMenu(EntitiesValues,'client',size=(10))],
                 [sg.Text("name:  "), sg.InputText(size=10)],
                 [sg.Text("port:   "), sg.InputText(size=10)],
                ]

EntityFieldsFrame = sg.Frame("",EntitiesFields, expand_x=True)
EntitiesFrame = sg.Frame("Entities - HTTP instances",layout=[[EntityFieldsFrame],[EntitiesListFrame]], expand_x=True)

main_window  = sg.Window(title=WINDOW_TITLE, layout=[[sg.Image(NERLNET_LOGO_PATH, expand_x=True)],
                                                     [sg.Text(f'Nerlnet ".json" files generator version {VERSION}')],
                                                     [fieldsFrame],[workersFrame, devicesFrame],
                                                     [EntitiesFrame]])

while True:
    event, values = main_window.read()
    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break
    print('You entered ', values[0])

main_window.close()