import PySimpleGUI as sg

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
                [sg.Text('Nerl GUI'), sg.Text('ip'), sg.InputText(size=15), sg.Text('port'), sg.InputText(size=15)],
                [sg.Button('Ok'), sg.Button('Cancel')] ]
fieldsFrame = sg.Frame("",layout=jsonFields)

# Devices 
DevicesNamesList = []
devicesListFields = [[sg.Text("Devices List")],
                     [sg.Listbox(DevicesNamesList, size=(25,15))]]
devicesListFrame = sg.Frame("", devicesListFields)
devicesFields = [[sg.Button("Add", size=(10)), sg.Button("Load",size=(10))],
                 [sg.Button("Ping",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:   "), sg.InputText(size=20)],
                 [sg.Text("ip:        "), sg.InputText(size=20)],
                 [sg.Text("port:     "), sg.InputText(size=20)],
                 [sg.Text("entities:"), sg.InputText(size=(20,5))]]

davicesFieldsFrame = sg.Frame("",devicesFields, expand_x=True)
devicesFrame = sg.Frame("Devices",layout=[[davicesFieldsFrame],[devicesListFrame]], expand_x=True)

# Workers 
workersNamesList = []
workersListFields = [[sg.Text("Workers List")],
                     [sg.Listbox(workersNamesList, size=(25,15))]]
workersListFrame = sg.Frame("", workersListFields)
workersFields = [
                 [sg.Button("Add", size=(10)), sg.Button("Load",size=(10))],
                 [sg.Button("Validate",size=(10)), sg.Button("Remove",size=(10))],
                 [sg.Text("name:   "), sg.InputText(size=10)],
                 [sg.Text("path to file of type *.wo/*.xml")],
                 [sg.InputText(size=22), sg.Button("Browse",size=(6))],
                 [sg.Button("Generate a new Worker",size=(30))],
                ]

workersFieldsFrame = sg.Frame("",workersFields, expand_x=True)
workersFrame = sg.Frame("Workers",layout=[[workersFieldsFrame],[workersListFrame]], expand_x=True)

main_window  = sg.Window(title=WINDOW_TITLE, layout=[[sg.Image(NERLNET_LOGO_PATH)],[fieldsFrame, workersFrame, devicesFrame]], margins=(360, 240))

while True:
    event, values = main_window.read()
    if event == sg.WIN_CLOSED or event == 'Cancel': # if user closes window or clicks cancel
        break
    print('You entered ', values[0])

main_window.close()