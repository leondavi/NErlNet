from pathlib import Path
import PySimpleGUI as sg
from JsonElementWorker import *
from WinWorkerDialogDefnitions import *
from JsonElementWorker import Worker

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
    return editable_list

def WinWorkerDialog():

    WorkerFileLayout = [[sg.Text("Load json"),sg.In(enable_events=True ,key=KEY_JSON_LOAD_FILE_BROWSE_EVENT, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),)),sg.Button("Load", key=KEY_JSON_FILE_LOAD_BUTTON_EVENT)],
                        [sg.Text("Select json file output directory"),sg.In(enable_events=True ,key=KEY_JSON_FILE_CHOSEN_DIR, expand_x=True), sg.FolderBrowse()],
                        [sg.Text("*.json file name"),sg.InputText(key=KEY_JSON_FILE_NAME, enable_events=True),sg.Button("Export",key=KEY_BUTTON_EXPORT_WORKER)]]
    WorkerFileFrame = sg.Frame("File",WorkerFileLayout)

    WorkerDefinitionsLayout = [[sg.Text("Model Type: "), sg.Combo(list(ModelTypeMapping.keys()),enable_events=True, key=KEY_MODEL_TYPE_LIST_BOX)],
                               [sg.Text("Layers Sizes: Comma separated list, # of neurons in a layer, E.g, 100,80,40,5,1")],
                               [sg.InputText(key=KEY_LAYER_SIZES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_SIZES)],
                               [sg.Text("List of layers types:"), sg.Combo(list(LayerTypeMap.keys()),key=KEY_LAYER_TYPE_SELECTION), sg.Button("Add",key=KEY_LAYER_TYPE_SELECTION_ADD), sg.Button("Help",key=KEY_LAYER_TYPE_HELP),sg.Button("Clear",key=KEY_LAYER_TYPE_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_LAYER_TYPE_CODES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_TYPES,enable_events=True)],
                               [sg.Text("Activation functions:"), sg.Combo(list(ActivationFunctionsMap.keys()),key=KEY_ACTIVATION_LAYER_SELECTION), sg.Button("Add",key=KEY_ACTIVATION_LAYER_SELECTION_ADD), sg.Button("Help",key="-ACTIVATION-LAYER-HELP-"), sg.Button("Clear",key=KEY_ACTIVATION_LAYER_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_ACTIVATION_CODES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_ACTIVATION_NUMOF_LAYERS,enable_events=True)]]
    WorkerDefinitionsFrame = sg.Frame("Model Definitions",layout=WorkerDefinitionsLayout)

    OptimizerDefinitionsLayout = [[sg.Text("Learning Rate: "), sg.InputText(key=KEY_LEARNING_RATE_INPUT, enable_events=True)],
                                  [sg.Text("Optimizer Type: "), sg.Combo(list(OptimizerTypeMapping.keys()),enable_events=True, key=KEY_OPTIMIZER_TYPE_LIST_BOX)],
                                  [sg.Text("Loss Method: "), sg.Combo(list(LossMethodMapping.keys()),enable_events=True, key=KEY_LOSS_METHOD_LIST_BOX)]]
    OptimizerDefinitionsFrame = sg.Frame("Optimizer Definitions", layout=OptimizerDefinitionsLayout)

 
    
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'New Worker Generator')],[WorkerFileFrame],[WorkerDefinitionsFrame],[OptimizerDefinitionsFrame]],modal=True, keep_on_top=True)                                                  
    choice = None

    # File Attributes
    filedir = ''
    filename = ''
    # Worker Attributes
    LayersSizesList = ""
    ModelTypeStr = ""
    ModelType = None # None
    OptimizationTypeStr = ""
    OptimizationType = None # None
    LossMethodStr = ""
    LossMethod = None # None
    LearningRate = None
    ActivationLayersList = ""
    LayerTypesList = ""
    while True:
        event, values = WorkerWindow.read()
        if event == KEY_JSON_FILE_CHOSEN_DIR:
            filedir = values[event]
        
        if event == KEY_JSON_FILE_NAME:
            filename = values[event]


        if event == KEY_MODEL_TYPE_LIST_BOX:
            ModelTypeStr = values[event]
            ModelType = ModelTypeMapping[ModelTypeStr]
            print(f"Model Id: {ModelType} {ModelTypeStr}")
        
        # Layers Sizes List
        if event == KEY_LAYER_SIZES_INPUT:
            LayersSizesList = values[event]
            WorkerWindow[KEY_NUM_OF_LAYERS_SIZES].update(f'({str(count_str_list_elements(LayersSizesList))})')

         # Layers Types output list handling:
        selection_key = KEY_LAYER_TYPE_SELECTION
        codes_key =  KEY_LAYER_TYPE_CODES_INPUT
        add_butt_key = KEY_LAYER_TYPE_SELECTION_ADD
        clear_butt_key = KEY_LAYER_TYPE_SELECTION_CLEAR
        LayerTypesList = combo_list_editable_handler(WorkerWindow, event, values, LayerTypeMap, LayerTypesList,
                                                    selection_key, codes_key, add_butt_key, clear_butt_key)
        WorkerWindow[KEY_NUM_OF_LAYERS_TYPES].update(f'({str(count_str_list_elements(LayerTypesList))})')

        if event == KEY_LAYER_TYPE_HELP:
            sg.popup_ok(f"Layer type codes:\n{pretty_print_dict(LayerTypeMap)}", keep_on_top=True, title="Layer Type Codes")


        # Activation codes combo and output list handling:
        selection_key = KEY_ACTIVATION_LAYER_SELECTION
        codes_key = KEY_ACTIVATION_CODES_INPUT
        add_butt_key = KEY_ACTIVATION_LAYER_SELECTION_ADD
        clear_butt_key = KEY_ACTIVATION_LAYER_SELECTION_CLEAR
        ActivationLayersList = combo_list_editable_handler(WorkerWindow, event, values, ActivationFunctionsMap, ActivationLayersList,
                                                            selection_key, codes_key, add_butt_key, clear_butt_key)
        WorkerWindow[KEY_ACTIVATION_NUMOF_LAYERS].update(f'({str(count_str_list_elements(ActivationLayersList))})')
        if event == KEY_ACTIVATION_LAYER_HELP:
            sg.popup_ok(f"Layer type codes:\n{pretty_print_dict(LayerTypeMap)}", keep_on_top=True, title="Layer Type Codes")

        if event == KEY_LEARNING_RATE_INPUT:
            LearningRate = values[event]
            print(f"selected Learning Rate {LearningRate}")

        if event == KEY_OPTIMIZER_TYPE_LIST_BOX:
            OptimizationTypeStr = values[event]
            OptimizationType = OptimizerTypeMapping[OptimizationTypeStr]
            print(f"selected Optimzier Type Id: {OptimizationType} {OptimizationTypeStr}")

        if event == KEY_LOSS_METHOD_LIST_BOX:
            LossMethodStr = values[event]
            LossMethod = LossMethodMapping[LossMethodStr]
            print(f"selected Loss Method Type: {LossMethod} {LossMethodStr}")

        if event == KEY_BUTTON_EXPORT_WORKER:
            worker_parameters_conditions = bool(LayersSizesList) and bool(ModelTypeStr) and bool(ModelType) and bool(OptimizationTypeStr) and\
                                           bool(OptimizationType) and bool(LossMethodStr) and bool(LossMethod) and\
                                           bool(LearningRate) and bool(ActivationLayersList) and bool(LayersSizesList)
            FilePath = Path(filedir) / Path(filename)
            filepath_condition = FilePath.parent.is_dir() and bool(filename) and filename.endswith(".json")
            if worker_parameters_conditions and filepath_condition:
                newWorker = Worker("new",LayersSizesList, ModelTypeStr, ModelType, OptimizationTypeStr, OptimizationType, LossMethodStr, LossMethod,
                                    LearningRate, ActivationLayersList, LayerTypesList)
                validation = newWorker.input_validation()
                print(f"validation: {validation}")
                print(f"Worker: {newWorker}")
                newWorker.save_as_json(FilePath.as_posix())
                sg.popup_auto_close("Successfully Created", keep_on_top=True)
                break
            elif not worker_parameters_conditions:
                sg.popup_auto_close("Missing Parameters!", keep_on_top=True)
            elif not filepath_condition:
                sg.popup_auto_close("Issue with json path!", keep_on_top=True)

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()