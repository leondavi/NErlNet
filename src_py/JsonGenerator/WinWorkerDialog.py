import PySimpleGUI as sg
from JsonElements import *

# Maps are based on src_cpp/opennnBridge/definitionsNN.h

LayerTypeMap = {
    "Default" : "0",
    "Scaling none" : "1-1",
    "Scaling MinMax" : "1-2",
    "Scaling MeanStd" : "1-3",
    "Scaling STD" : "1-4",
    "Scaling Log" : "1-5",
    "CNN" : "2",
    "Perceptron" : "3",
    "Pooling none" : "4-1",
    "Pooling max" : "4-2",
    "Pooling avg" : "4-3",
    "Probabilistic" : "5",
    "LSTM" : "6",
    "RNN" : "7",
    "Unscaling" : "8",
    "Bounding" : "9"
}

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

OptimizerTypeMapping = {
    "none" : 0,
    "SGD" : 1,
    "Mini-Batch" : 2,
    "Momentum" : 3,
    "NAG" : 4,
    "Adagrad" : 5,
    "ADAM" : 6
}

LossMethodMapping = {
    "SSE" : 1, # Sum squared Error
    "MSE" : 2, # Mean Squared Error
    "NSE" : 3, # Normalized Squared Error
    "Minkowski-E" : 4, # Minkowski Error
    "WSE" : 5, # Weighted Squared Error
    "CEE" : 6, # Cross Entropy Error
}

def count_str_list_elements(list_str : str):
    return len(list_str.split(',')) if list_str else 0

def pretty_print_dict(d):#define d
    pretty_dict = ''  #take empty string
    for k, v in d.items():#get items for dict
        pretty_dict += f'{k}: {str(v)}\n'
    return pretty_dict#return result

def combo_list_editable_handler(window, event, values, map, editable_list, selection_key, codes_key, add_butt_key, clear_butt_key):
    if values:
        editable_list = values[codes_key]
        if  event == add_butt_key:   
            editable_list = editable_list + "," if editable_list else editable_list
            if values[selection_key]:
                editable_list += str(map[values[selection_key]])
                window[codes_key].update(editable_list)
        elif event == clear_butt_key:
            editable_list = ""
            window[codes_key].update(editable_list)
    return editable_list

def WinWorkerDialog():
    WorkerDefinitionsLayout = [[sg.Text("Model Type: "), sg.Combo(list(ModelTypeMapping.keys()),enable_events=True, key='-MODEL-TYPE-LIST-BOX-')],
                               [sg.Text("Layers Sizes: Comma separated list, # of neurons in a layer, E.g, 100,80,40,5,1")],
                               [sg.InputText(key="-LAYERS-SIZES-INPUT-",enable_events=True), sg.Text("(0)",key="-NUM-OF-LAYERS-SIZES-")],
                               [sg.Text("List of layers types:"), sg.Combo(list(LayerTypeMap.keys()),key='-LAYER-TYPE-SELECTION-'), sg.Button("Add",key="-LAYER-TYPE-SELECTION-ADD-"), sg.Button("Help",key="-LAYER-TYPE-HELP-"),sg.Button("Clear",key="-LAYER-TYPE-SELECTION-CLEAR-")],
                               [sg.InputText(key="-LAYER-TYPE-CODES-INPUT-",enable_events=True), sg.Text("(0)",key="-NUM-OF-LAYERS-TYPES-",enable_events=True)],
                               [sg.Text("Activation functions:"), sg.Combo(list(ActivationFunctionsMap.keys()),key='-ACTIVATION-LAYER-SELECTION-'), sg.Button("Add",key="-ACTIVATION-LAYER-SELECTION-ADD-"), sg.Button("Help",key="-ACTIVATION-LAYER-HELP-"), sg.Button("Clear",key="-ACTIVATION-LAYER-SELECTION-CLEAR-")],
                               [sg.InputText(key="-ACTIVATION-CODES-INPUT-",enable_events=True), sg.Text("(0)",key="-NUM-OF-LAYERS-ACTIVATIONS-",enable_events=True)]]
    WorkerDefinitionsFrame = sg.Frame("Model Definitions",layout=WorkerDefinitionsLayout)

    OptimizerDefinitionsLayout = [[sg.Text("Learning Rate: "), sg.InputText(key="-LEARNING-RATE-INPUT-", enable_events=True)],
                                  [sg.Text("Optimizer Type: "), sg.Combo(list(OptimizerTypeMapping.keys()),enable_events=True, key='-OPTIMIZER-TYPE-LIST-BOX-')],
                                  [sg.Text("Loss Method: "), sg.Combo(list(LossMethodMapping.keys()),enable_events=True, key='-LOSS-METHOD-LIST-BOX-')]]
    OptimizerDefinitionsFrame = sg.Frame("Optimizer Definitions", layout=OptimizerDefinitionsLayout)

    WorkerFileLayout = [[sg.Text("Select json file output directory"),sg.In(enable_events=True ,key="-XO-FILE-CHOSEN-DIRECTORY", expand_x=True), sg.FolderBrowse()],
                        [sg.Text("*.json file name"),sg.InputText(key="-JSON-FILE-NAME-"),sg.Button("Export")]]
    WorkerFileFrame = sg.Frame("File",WorkerFileLayout)
    
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'New Worker Generator')],[WorkerDefinitionsFrame],[OptimizerDefinitionsFrame],[WorkerFileFrame]],modal=True, keep_on_top=True)                                                  
    choice = None

    LayersSizesList = ""
    ModelTypeStr = ""
    ModelType = -1 # None
    OptimizationTypeStr = ""
    OptimizationType = -1 # None
    LossMethodStr = ""
    LossMethod = -1 # None
    LearningRate = 0
    ActivationLayersList = ""
    LayerTypesList = ""
    while True:
        event, values = WorkerWindow.read()
        if event == "-MODEL-TYPE-LIST-BOX-":
            ModelTypeStr = values[event]
            ModelType = ModelTypeMapping[ModelTypeStr]
            print(f"Model Id: {ModelType} {ModelTypeStr}")
        
        # Layers Sizes List
        if event == "-LAYERS-SIZES-INPUT-":
            LayersSizesList = values[event]
            WorkerWindow["-NUM-OF-LAYERS-SIZES-"].update(f'({str(count_str_list_elements(LayersSizesList))})')


        #TODO layers sizes

         # Layers Types output list handling:
        selection_key = '-LAYER-TYPE-SELECTION-'
        codes_key = '-LAYER-TYPE-CODES-INPUT-'
        add_butt_key = '-LAYER-TYPE-SELECTION-ADD-'
        clear_butt_key = '-LAYER-TYPE-SELECTION-CLEAR-'
        LayerTypesList = combo_list_editable_handler(WorkerWindow, event, values, LayerTypeMap, LayerTypesList,
                                                    selection_key, codes_key, add_butt_key, clear_butt_key)
        WorkerWindow["-NUM-OF-LAYERS-TYPES-"].update(f'({str(count_str_list_elements(LayerTypesList))})')

        if event == "-LAYER-TYPE-HELP-":
            sg.popup_ok(f"Layer type codes:\n{pretty_print_dict(LayerTypeMap)}", keep_on_top=True, title="Layer Type Codes")


        # Activation codes combo and output list handling:
        selection_key = '-ACTIVATION-LAYER-SELECTION-'
        codes_key = '-ACTIVATION-CODES-INPUT-'
        add_butt_key = '-ACTIVATION-LAYER-SELECTION-ADD-'
        clear_butt_key = '-ACTIVATION-LAYER-SELECTION-CLEAR-'
        ActivationLayersList = combo_list_editable_handler(WorkerWindow, event, values, ActivationFunctionsMap, ActivationLayersList,
                                                            selection_key, codes_key, add_butt_key, clear_butt_key)
        WorkerWindow["-NUM-OF-LAYERS-ACTIVATIONS-"].update(f'({str(count_str_list_elements(ActivationLayersList))})')
        if event == "-LAYER-TYPE-HELP-":
            sg.popup_ok(f"Layer type codes:\n{pretty_print_dict(LayerTypeMap)}", keep_on_top=True, title="Layer Type Codes")

        if event == "-LEARNING-RATE-INPUT-":
            LearningRate = values[event]
            print(f"selected Learning Rate {LearningRate}")

        if event == "-OPTIMIZER-TYPE-LIST-BOX-":
            OptimizationTypeStr = values[event]
            OptimizationType = OptimizerTypeMapping[OptimizationTypeStr]
            print(f"selected Optimzier Type Id: {OptimizationType} {OptimizationTypeStr}")

        if event == "-LOSS-METHOD-LIST-BOX-":
            LossMethodStr = values[event]
            LossMethod = LossMethodMapping[LossMethodStr]
            print(f"selected Loss Method Type: {LossMethod} {LossMethodStr}")

        if event == "Export":
            newWorker = Worker("new", LayersSizesList, ModelTypeStr, ModelType, LossMethod, LearningRate, LossMethodStr, LossMethod, LearningRate, ActivationLayersList, LayerTypesList )
            validation = newWorker.input_validation()
            print(f"validation: {validation}")
            sg.popup_auto_close("Successfully Created", keep_on_top=True)
            break

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()