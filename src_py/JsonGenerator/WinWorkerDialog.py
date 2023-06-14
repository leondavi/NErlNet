import PySimpleGUI as sg
from JsonElements import *

ActivationFunctionsMap = {
    "Threshold" : 1,
    "Sign" : 2,
    "Logistic" : 3,
    "Tanh" : 4,
    "Linear" : 5,
    "ReLU" : 6,
    "eLU" : 7,
    "SeLU" : 8,
    "Soft-plus" : 9,
    "Soft-sign" : 10,
    "Hard-sigmoid" : 11,
}

ModelTypeMapping = {
    "approximation" : 1,
    "classification" : 2,
    "forecasting" : 3,
    "encoder_decoder" : 4,
    "nn" : 5,
    "autoencoder" : 6,
    "ae-classifier" : 7,
    "fed-client": 8,
    "fed-server": 9
}

def WinWorkerDialog():
    
    WorkerDefinitionsLayout = [[sg.Text("Model Type: "), sg.Combo(list(ModelTypeMapping.keys()),enable_events=True, key='-MODEL-TYPE-LIST-BOX-')],
                               [sg.Text("Layers Sizes - Comma separated list of integers, E.g, 100,80,40,5,1")],
                               [sg.InputText(key="-LAYERS-SIZES-INPUT-")],
                               [sg.Text("Activation functions codes per each layer (use List and Add or type). E.g, 5,6,11,11,5"), sg.Combo(list(ActivationFunctionsMap.keys()),key='-ACTIVATION-LAYER-SELECTION-'), sg.Button("Add",key="-ACTIVATION-LAYER-SELECTION-ADD-"), sg.Button("Clear",key="-ACTIVATION-LAYER-SELECTION-CLEAR-")],
                               [sg.InputText(key="-ACTIVATION-CODES-INPUT-")]]
    WorkerDefinitionsFrame = sg.Frame("Model Definitions",layout=WorkerDefinitionsLayout)

    WorkerFileLayout = [[sg.Text("Select xo file output directory"),sg.In(enable_events=True ,key="-XO-FILE-CHOSEN-DIRECTORY", expand_x=True), sg.FolderBrowse()],
                        [sg.Text("*.xo file name"),sg.InputText(key="-XO-FILE-NAME-"),sg.Button("Generate")]]
    WorkerFileFrame = sg.Frame("File",WorkerFileLayout)
    
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'New Worker Generator')],[WorkerDefinitionsFrame],[WorkerFileFrame]],modal=True, keep_on_top=True)                                                  
    choice = None

    ModelTypeStr = ""
    ActivationLayersList = ""
    while True:
        event, values = WorkerWindow.read()
        if event == "-MODEL-TYPE-LIST-BOX-":
            ModelTypeStr = values[event]
            ModelType = ModelTypeMapping[ModelTypeStr]
            print(f"Model Id: {ModelType} {ModelTypeStr}")

        # Activation codes cases:
        elif event == "-ACTIVATION-LAYER-SELECTION-ADD-":
            if values['-ACTIVATION-CODES-INPUT-']:
                ActivationLayersList = values['-ACTIVATION-CODES-INPUT-']
            ActivationLayersList = ActivationLayersList + "," if ActivationLayersList else ActivationLayersList
            ActivationLayersList += str(ActivationFunctionsMap[values['-ACTIVATION-LAYER-SELECTION-']])
            WorkerWindow['-ACTIVATION-CODES-INPUT-'].update(ActivationLayersList)
        elif event == "-ACTIVATION-LAYER-SELECTION-CLEAR-":
            ActivationLayersList = ""
            WorkerWindow['-ACTIVATION-CODES-INPUT-'].update(ActivationLayersList)
        elif event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()