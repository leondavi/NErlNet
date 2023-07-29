from pathlib import Path
import PySimpleGUI as sg
from JsonElementWorker import *
from WinWorkerDialogDefnitions import *
from JsonElementWorker import Worker
from Definitions import *

global_layer_method_selection_code = None

def count_str_list_elements(list_str : str):
    return len(list_str.split(',')) if list_str else 0

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
    global global_layer_method_selection

    WorkerFileLayout = [[sg.Text("Load json"),sg.In(enable_events=True ,key=KEY_JSON_LOAD_FILE_BROWSE_EVENT, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),)), sg.Button("Load", key=KEY_JSON_LOAD_FILE_BUTTON_EVENT, enable_events=True)],
                        [sg.Text("Select json file output directory"),sg.In(enable_events=True ,key=KEY_JSON_FILE_CHOSEN_DIR, expand_x=True), sg.FolderBrowse()],
                        [sg.Text("*.json file name"),sg.InputText(key=KEY_JSON_FILE_NAME, enable_events=True),sg.Button("Export",key=KEY_BUTTON_EXPORT_WORKER), sg.Checkbox('with documentation', default=True, key=KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION, enable_events=True)]]
    WorkerFileFrame = sg.Frame("File",WorkerFileLayout)

    WorkerDefinitionsLayout = [[sg.Text("Model Type: "), sg.Combo(list(ModelTypeMapping.keys()),enable_events=True, key=KEY_MODEL_TYPE_LIST_BOX), sg.Button("LSTM Opts",disabled=True), sg.Button("CNN Opts", disabled=True)],
                               [sg.Text("Layers Sizes: Comma separated list, # of neurons in a layer, E.g, 100,80,40,5,1")],
                               [sg.InputText(key=KEY_LAYER_SIZES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_SIZES)],
                               [sg.Text("List of layers types:"), sg.Combo(list(LayerTypeMap.keys()),key=KEY_LAYER_TYPE_SELECTION), sg.Button("Add",key=KEY_LAYER_TYPE_SELECTION_ADD), sg.Button("Help",key=KEY_LAYER_TYPE_HELP),sg.Button("Clear",key=KEY_LAYER_TYPE_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_LAYER_TYPE_CODES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_TYPES,enable_events=True)],
                               [sg.Text("Layers Functionality-Codes"),sg.Button("Select Layer Method", enable_events=True, key=KEY_LAYER_METHODS_BUTTON_SELECT), sg.Button("Help",key="-ACTIVATION-LAYER-HELP-"), sg.Button("Clear",key=KEY_LAYER_FUNCTIONS_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_LAYER_FUNCTIONS_CODES_INPUT,enable_events=True), sg.Text("(0)",key=KEY_LAYERS_FUNCTIONS_CODES,enable_events=True), sg.Text("",enable_events=True, key=KEY_LAYER_METHODS_TEXT_SELECTION)]]
    WorkerDefinitionsFrame = sg.Frame("Model Definitions",layout=WorkerDefinitionsLayout)

    OptimizerDefinitionsLayout = [[sg.Text("Learning Rate: "), sg.InputText(key=KEY_LEARNING_RATE_INPUT, enable_events=True)],
                                  [sg.Text("Optimizer Type: "), sg.Combo(list(OptimizerTypeMapping.keys()),enable_events=True, key=KEY_OPTIMIZER_TYPE_LIST_BOX)],
                                  [sg.Text("Loss Method: "), sg.Combo(list(LossMethodMapping.keys()),enable_events=True, key=KEY_LOSS_METHOD_LIST_BOX)]]
    OptimizerDefinitionsFrame = sg.Frame("Optimizer Definitions", layout=OptimizerDefinitionsLayout)

 
    
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'New Worker Generator')],[WorkerFileFrame],[WorkerDefinitionsFrame],[OptimizerDefinitionsFrame]],modal=True, keep_on_top=True)                                                  

    # File Attributes
    FileDirExport = ''
    FileNameExport = ''
    # Load File
    FilePathLoad = ''
    # Worker Attributes
    LayersSizesList = ""
    ModelTypeStr = ""
    ModelType = None # None
    OptimizationTypeStr = ""
    OptimizationType = None # None
    LossMethodStr = ""
    LossMethod = None # None
    LearningRate = None
    LayersFunctionsList = ""
    LayerTypesList = ""
    WithDocumentation = True

    def ui_update_all_values(WorkerWindow):
        WorkerWindow[KEY_LAYER_SIZES_INPUT].update(LayersSizesList)
        WorkerWindow[KEY_MODEL_TYPE_LIST_BOX].update(ModelTypeStr)
        WorkerWindow[KEY_OPTIMIZER_TYPE_LIST_BOX].update(OptimizationTypeStr)
        WorkerWindow[KEY_LOSS_METHOD_LIST_BOX].update(LossMethodStr)
        WorkerWindow[KEY_LEARNING_RATE_INPUT].update(LearningRate)
        WorkerWindow[KEY_LAYER_FUNCTIONS_CODES_INPUT].update(LayersFunctionsList)
        WorkerWindow[KEY_LAYER_TYPE_CODES_INPUT].update(LayerTypesList)
        # update counters
        WorkerWindow[KEY_NUM_OF_LAYERS_SIZES].update(f'({str(count_str_list_elements(LayersSizesList))})')
        WorkerWindow[KEY_NUM_OF_LAYERS_TYPES].update(f'({str(count_str_list_elements(LayerTypesList))})')
        WorkerWindow[KEY_LAYERS_FUNCTIONS_CODES].update(f'({str(count_str_list_elements(LayersFunctionsList))})')


    while True:
        event, values = WorkerWindow.read()
        if event == KEY_JSON_FILE_CHOSEN_DIR:
            FileDirExport = values[event]
        
        if event == KEY_JSON_FILE_NAME:
            FileNameExport = values[event]


        if event == KEY_MODEL_TYPE_LIST_BOX:
            ModelTypeStr = values[event]
            ModelType = ModelTypeMapping[ModelTypeStr]
        
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


        selection_key = KEY_ACTIVATION_LAYER_SELECTION
        codes_key = KEY_LAYER_FUNCTIONS_CODES_INPUT
        add_butt_key = None
        clear_butt_key = KEY_LAYER_FUNCTIONS_SELECTION_CLEAR
        LayersFunctionsList = combo_list_editable_handler(WorkerWindow, event, values, ActivationFunctionsMap, LayersFunctionsList,
                                                            selection_key, codes_key, add_butt_key, clear_butt_key)
        WorkerWindow[KEY_LAYERS_FUNCTIONS_CODES].update(f'({str(count_str_list_elements(LayersFunctionsList))})')

        # Activation codes combo and output list handling:
        if event == KEY_LAYER_METHODS_BUTTON_SELECT:
            LayerMethodSelection()
            LayersFunctionsList += ',' if not LayersFunctionsList.endswith(',') and LayersFunctionsList else ''
            LayersFunctionsList += global_layer_method_selection_code
            WorkerWindow[KEY_LAYER_FUNCTIONS_CODES_INPUT].update(LayersFunctionsList)
            WorkerWindow[KEY_LAYERS_FUNCTIONS_CODES].update(f'({str(count_str_list_elements(LayersFunctionsList))})')



        if event == KEY_ACTIVATION_LAYER_HELP:
            ActivationDictStr = f'Activation:\n{pretty_print_dict(ActivationFunctionsMap)}'
            PoolingDictStr = f'Pooling:\n{pretty_print_dict(PoolingMethodMap)}'
            ScalerDictStr = f'Scaler:\n{pretty_print_dict(ScalingMethodMap)}'
            ProbabilisticDictStr = f'Probabilistic:\n{pretty_print_dict(ProbabilisticActivationFunctionMap)}'
            sg.popup_ok(f"Layer Functions Codes:\n{ActivationDictStr}\n{PoolingDictStr}\n{ScalerDictStr}\n{ProbabilisticDictStr}", keep_on_top=True, title="Layer Type Codes")

        if event == KEY_LEARNING_RATE_INPUT:
            LearningRate = values[event]

        if event == KEY_OPTIMIZER_TYPE_LIST_BOX:
            OptimizationTypeStr = values[event]
            OptimizationType = OptimizerTypeMapping[OptimizationTypeStr]

        if event == KEY_LOSS_METHOD_LIST_BOX:
            LossMethodStr = values[event]
            LossMethod = LossMethodMapping[LossMethodStr]

        if event == KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION:
            WithDocumentation = values[KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION]

        if event == KEY_BUTTON_EXPORT_WORKER:
            worker_parameters_conditions = bool(LayersSizesList) and bool(ModelTypeStr) and bool(ModelType) and bool(OptimizationTypeStr) and\
                                           bool(OptimizationType) and bool(LossMethodStr) and bool(LossMethod) and\
                                           bool(LearningRate) and bool(LayersFunctionsList) and bool(LayersSizesList)
            FilePath = Path(FileDirExport) / Path(FileNameExport)
            filepath_condition = FilePath.parent.is_dir() and bool(FileNameExport) and FileNameExport.endswith(".json")
            if worker_parameters_conditions and filepath_condition:
                newWorker = Worker("new",LayersSizesList, ModelTypeStr, ModelType, OptimizationTypeStr, OptimizationType, LossMethodStr, LossMethod,
                                    LearningRate, LayersFunctionsList, LayerTypesList)
                newWorker.save_as_json(FilePath.as_posix(), WithDocumentation)
                sg.popup_auto_close("Successfully Created", keep_on_top=True)
                break
            elif not worker_parameters_conditions:
                sg.popup_auto_close("Missing Parameters!", keep_on_top=True)
            elif not filepath_condition:
                sg.popup_auto_close("Issue with json path!", keep_on_top=True)

        if event == KEY_JSON_LOAD_FILE_BROWSE_EVENT:
            FilePathLoad = values[KEY_JSON_LOAD_FILE_BROWSE_EVENT]
            print(f"{FilePathLoad}")
        
        if event == KEY_JSON_LOAD_FILE_BUTTON_EVENT:
            load_conditions = bool(FilePathLoad) and FilePathLoad.endswith(".json")
            if load_conditions:
                # loading json
                loaded_worker_dict = {}
                with open(FilePathLoad) as jsonFile:
                    loaded_worker_dict = json.load(jsonFile)
                ( _ , LayersSizesList, ModelTypeStr, ModelType, OptimizationTypeStr,
                OptimizationType, LossMethodStr, LossMethod, LearningRate, LayersFunctionsList, LayerTypesList) = Worker.load_from_dict(loaded_worker_dict)
                ui_update_all_values(WorkerWindow)

            else:
                sg.popup_auto_close("Issue with selected json file", keep_on_top=True)

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()


def LayerMethodSelection():
    global global_layer_method_selection_code

    layout = [[sg.Text("Activation",expand_x=True), sg.Text('Pooling', expand_x=True), sg.Text('Scaler', expand_x=True), sg.Text('Probabilistic', expand_x=True)],
                [sg.Listbox(list(ActivationFunctionsMap.keys()), size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_ACTIVATION),
                 sg.Listbox(list(PoolingMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_POOLING), 
                 sg.Listbox(list(ScalingMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_SCALER),
                 sg.Listbox(list(ProbabilisticActivationFunctionMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_PROBABILISTIC)],
                 [sg.Text('Selection', expand_x=True, enable_events=True, key=KEY_LAYER_METHOD_SELECTION_TEXT),sg.Button('Select', expand_x=True, key=KEY_LAYER_METHOD_SELECTION_BUTTON)]]
    
    layer_selection_win = sg.Window(title="Layer Method Selection", layout=layout, modal=True, keep_on_top=True)


    while True: 
        event, values = layer_selection_win.read(timeout=100)

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_ACTIVATION:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_ACTIVATION][0]
            global_layer_method_selection_code = ActivationFunctionsMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_POOLING:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_POOLING][0]
            global_layer_method_selection_code = PoolingMethodMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_SCALER:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_SCALER][0]
            global_layer_method_selection_code = ScalingMethodMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_PROBABILISTIC:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_PROBABILISTIC][0]
            global_layer_method_selection_code = ProbabilisticActivationFunctionMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_BUTTON:
            break
        

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
    
    layer_selection_win.close()
