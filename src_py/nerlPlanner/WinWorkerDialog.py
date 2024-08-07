from pathlib import Path
import PySimpleGUI as sg
from JsonElementWorker import *
from WinWorkerDialogDefnitions import *
from JsonElementWorker import Worker
from Definitions import *
import random 
import string

global_layer_method_selection_code = None

def count_str_list_elements(list_str : str):
    return len(list_str.split(',')) if list_str else 0

def combo_list_editable_handler(window, event, values, map, editable_list, selection_key, codes_key, add_butt_key, clear_butt_key):
    if values:
        editable_list = values[codes_key]
        if  event == add_butt_key:   
            editable_list = editable_list + "," if editable_list else editable_list
            if (selection_key in values) and (values[selection_key]):
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

    WorkerDefinitionsLayout = [[sg.Text("Model Type: "), sg.Combo(list(ModelTypeMapping.keys()),enable_events=True, key=KEY_MODEL_TYPE_LIST_BOX), sg.Button("LSTM Opts",disabled=True), sg.Button("Layers Sizes Help", enable_events=True, key=KEY_LAYER_SIZES_HELP_BUTTON)],
                               [sg.Text("Model Args:"), sg.InputText(key=KEY_MODEL_ARGS_INPUT, enable_events=True, expand_x=True) , sg.Text("(0)",key=KEY_NUM_OF_MODEL_ARGS)],
                               [sg.Text("Layers Sizes: Comma separated list, # of neurons in a layer, E.g, 100,80,40,5,1")],
                               [sg.InputText(key=KEY_LAYER_SIZES_INPUT,enable_events=True, expand_x=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_SIZES)],
                               [sg.Text("List of layers types:"), sg.Combo(list(LayerTypeMap.keys()),key=KEY_LAYER_TYPE_SELECTION), sg.Button("Add",key=KEY_LAYER_TYPE_SELECTION_ADD), sg.Button("Help",key=KEY_LAYER_TYPE_HELP),sg.Button("Clear",key=KEY_LAYER_TYPE_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_LAYER_TYPE_CODES_INPUT,enable_events=True, expand_x=True), sg.Text("(0)",key=KEY_NUM_OF_LAYERS_TYPES,enable_events=True)],
                               [sg.Text("Layers Functionality-Codes"),sg.Button("Select Layer Method", enable_events=True, key=KEY_LAYER_METHODS_BUTTON_SELECT), sg.Button("Help",key="-ACTIVATION-LAYER-HELP-"), sg.Button("Clear",key=KEY_LAYER_FUNCTIONS_SELECTION_CLEAR)],
                               [sg.InputText(key=KEY_LAYER_FUNCTIONS_CODES_INPUT,enable_events=True, expand_x=True), sg.Text("(0)",key=KEY_LAYERS_FUNCTIONS_CODES,enable_events=True)]]
    WorkerDefinitionsFrame = sg.Frame("Model Definitions",layout=WorkerDefinitionsLayout, expand_x=True)

    OptimizerDefinitionsLayout = [[sg.Text("Learning Rate: "), sg.InputText(key=KEY_LEARNING_RATE_INPUT, size=(15), enable_events=True)],
                                  [sg.Text("Epochs:          "), sg.InputText("1", size=(15), key=KEY_EPOCHS_INPUT, enable_events=True)],
                                  [sg.Text("Optimizer Type: "), sg.Combo(list(OptimizerTypeMapping.keys()),enable_events=True, key=KEY_OPTIMIZER_TYPE_LIST_BOX),
                                   sg.Text("   Optimizer Args:"), sg.InputText(key=KEY_OPTIMIZER_ARGS_INPUT, size=(15), enable_events=True, expand_x=True)],
                                  [sg.Text("Loss Method: "), sg.Combo(list(LossMethodMapping.keys()),enable_events=True, key=KEY_LOSS_METHOD_LIST_BOX),
                                   sg.Text("   Loss Args:"), sg.InputText(key=KEY_LOSS_ARGS_INPUT, size=(15), enable_events=True, expand_x=True)]]

    OptimizerDefinitionsFrame = sg.Frame("Optimizer Definitions", layout=OptimizerDefinitionsLayout, expand_x=True)

    InfraTypeLayout = [[ sg.Text("Infra Type: "), sg.Combo(list(InfraTypeMapping.keys()),default_value=list(InfraTypeMapping.keys())[0], enable_events=True, key=KEY_INFRA_TYPE_LIST_BOX)],
                       [ sg.Text("Distributed System Type:"), sg.Combo(list(DistributedSystemTypeMapping.keys()),default_value=list(DistributedSystemTypeMapping.keys())[0], enable_events=True, key=KEY_DISTRIBUTED_SYSTEM_TYPE_LIST_BOX)],
                       [ sg.Text("Distributed System Token:"), sg.InputText(key=KEY_DISTRIBUTED_SYSTEM_TOKEN_INPUT, enable_events=True, expand_x=True), sg.Button("AutoGenerate", key=KEY_DISTRIBUTED_SYSTEM_TOKEN_AUTOGENERATE_BUTTON), sg.Button("Help", key=KEY_DISTRIBUTED_SYSTEM_TOKEN_HELP_BUTTON)],
                       [sg.Text("Distributed System Args:"), sg.InputText(key=KEY_DISTRIBUTED_SYSTEM_ARGS_INPUT, enable_events=True, expand_x=True)]]
    InfraTypeFrame = sg.Frame("Worker Distributed System", layout=InfraTypeLayout, expand_x=True)
    
    WorkerWindow  = sg.Window(title="Worker", layout=[[sg.Text(f'New Worker Generator')],[WorkerFileFrame],[WorkerDefinitionsFrame],[OptimizerDefinitionsFrame], [InfraTypeFrame]],modal=True, keep_on_top=True)                                                  

    # File Attributes
    FileDirExport = ''
    FileNameExport = ''
    # Load File
    FilePathLoad = ''
    # Worker Attributes
    LayersSizesList = ""
    ModelTypeStr = ""
    ModelType = None # None
    ModelArgsStr = ""
    OptimizationType = list(OptimizerTypeMapping.keys())[0]
    OptimizationArgs = "none"
    LossMethodStr = ""
    LossMethod = None # None
    LossArgs = "none"
    LearningRate = None
    Epochs = "1"
    LayersFunctionsList = ""
    LayerTypesList = ""
    WithDocumentation = True
    InfraType = list(InfraTypeMapping.keys())[0]
    DistributedSystemType = list(DistributedSystemTypeMapping.keys())[0]
    DistributedSystemArgs = "none"
    DistributedSystemToken = "none"

    def ui_update_all_values(WorkerWindow):
        # Update here when adding new fields to the worker
        WorkerWindow[KEY_LAYER_SIZES_INPUT].update(LayersSizesList)
        WorkerWindow[KEY_MODEL_TYPE_LIST_BOX].update(ModelTypeStr)
        WorkerWindow[KEY_MODEL_ARGS_INPUT].update(ModelArgsStr)
        WorkerWindow[KEY_OPTIMIZER_TYPE_LIST_BOX].update(OptimizationType)
        WorkerWindow[KEY_OPTIMIZER_ARGS_INPUT].update(OptimizationArgs)
        WorkerWindow[KEY_INFRA_TYPE_LIST_BOX].update(InfraType)
        WorkerWindow[KEY_LOSS_METHOD_LIST_BOX].update(LossMethodStr)
        WorkerWindow[KEY_LOSS_ARGS_INPUT].update(LossArgs)
        WorkerWindow[KEY_LEARNING_RATE_INPUT].update(LearningRate)
        WorkerWindow[KEY_EPOCHS_INPUT].update(Epochs)
        WorkerWindow[KEY_LAYER_FUNCTIONS_CODES_INPUT].update(LayersFunctionsList)
        WorkerWindow[KEY_LAYER_TYPE_CODES_INPUT].update(LayerTypesList)
        WorkerWindow[KEY_DISTRIBUTED_SYSTEM_TYPE_LIST_BOX].update(DistributedSystemType)
        WorkerWindow[KEY_DISTRIBUTED_SYSTEM_ARGS_INPUT].update(DistributedSystemArgs)
        WorkerWindow[KEY_DISTRIBUTED_SYSTEM_TOKEN_INPUT].update(DistributedSystemToken)
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

        if event == KEY_MODEL_ARGS_INPUT:
            ModelArgsStr = values[event]
            WorkerWindow[KEY_NUM_OF_MODEL_ARGS].update(f'({str(count_str_list_elements(ModelArgsStr))})')
        
        if event == KEY_LAYER_SIZES_HELP_BUTTON:
            sg.popup_ok(f"{LAYER_SIZES_HELP_POPUP_STR}", keep_on_top=True, title="Layer Sizes Help")

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
        if event: # Protects from update when windows is closed
            WorkerWindow[KEY_NUM_OF_LAYERS_TYPES].update(f'({str(count_str_list_elements(LayerTypesList))})') if LayerTypesList else None

        if event == KEY_LAYER_TYPE_HELP:
            sg.popup_ok(f"Layer type codes:\n{pretty_print_dict(LayerTypeMap)}", keep_on_top=True, title="Layer Type Codes")


        selection_key = None
        codes_key = KEY_LAYER_FUNCTIONS_CODES_INPUT
        add_butt_key = None
        clear_butt_key = KEY_LAYER_FUNCTIONS_SELECTION_CLEAR
        LayersFunctionsList = combo_list_editable_handler(WorkerWindow, event, values, ActivationFunctionsMap, LayersFunctionsList,
                                                            selection_key, codes_key, add_butt_key, clear_butt_key)

        if event: # Protects from update when windows is closed
            WorkerWindow[KEY_LAYERS_FUNCTIONS_CODES].update(f'({str(count_str_list_elements(LayersFunctionsList))})') if LayersFunctionsList else None

        # Activation codes combo and output list handling:
        if event == KEY_LAYER_METHODS_BUTTON_SELECT:
            LayerMethodSelection()
            LayersFunctionsList += ',' if not LayersFunctionsList.endswith(',') and LayersFunctionsList else ''
            LayersFunctionsList += global_layer_method_selection_code if global_layer_method_selection_code else ''
            WorkerWindow[KEY_LAYER_FUNCTIONS_CODES_INPUT].update(LayersFunctionsList)
            WorkerWindow[KEY_LAYERS_FUNCTIONS_CODES].update(f'({str(count_str_list_elements(LayersFunctionsList))})') if LayersFunctionsList else None

        if event == KEY_ACTIVATION_LAYER_HELP:
            ActivationDictStr = f'Activation:\n{pretty_print_dict(ActivationFunctionsMap)}'
            PoolingDictStr = f'Pooling:\n{pretty_print_dict(PoolingMethodMap)}'
            ScalerDictStr = f'Scaler:\n{pretty_print_dict(ScalingMethodMap)}'
            FlattenDictStr = f'Flatten:\n{pretty_print_dict(FlattenMethodMap)}'
            BoundingDictStr = f'Bounding:\n{pretty_print_dict(BoundingMethodMap)}'
            ProbabilisticDictStr = f'Probabilistic:\n{pretty_print_dict(ProbabilisticActivationFunctionMap)}'
            sg.popup_ok(f"Layer Functions Codes:\n{ActivationDictStr}\n{PoolingDictStr}\n{ScalerDictStr}\n{FlattenDictStr}\n{BoundingDictStr}\n{ProbabilisticDictStr}", keep_on_top=True, title="Layer Type Codes")

        if event == KEY_LEARNING_RATE_INPUT:
            LearningRate = values[event]

        if event == KEY_EPOCHS_INPUT:
            Epochs = values[event]

        if event == KEY_OPTIMIZER_TYPE_LIST_BOX:
            OptimizationType = values[event]
        
        if event == KEY_OPTIMIZER_ARGS_INPUT:
            OptimizationArgs = values[event]

        if event == KEY_INFRA_TYPE_LIST_BOX:
            InfraType = values[event]

        if event == KEY_DISTRIBUTED_SYSTEM_TYPE_LIST_BOX:
            DistributedSystemType = values[event]
            
        if event == KEY_DISTRIBUTED_SYSTEM_ARGS_INPUT:
            DistributedSystemArgs = values[event]

        if event == KEY_DISTRIBUTED_SYSTEM_TOKEN_INPUT:
            DistributedSystemToken = values[event]

        if event == KEY_DISTRIBUTED_SYSTEM_TOKEN_AUTOGENERATE_BUTTON:
            # Generate 4 random digits
            digits = ''.join(random.choices(string.digits, k=4))
            # Generate 1 random letter
            letter = random.choice(string.ascii_letters)
            DistributedSystemToken = digits + letter
            ui_update_all_values(WorkerWindow)
            
        if event == KEY_LOSS_METHOD_LIST_BOX:
            LossMethodStr = values[event]
            LossMethod = LossMethodMapping[LossMethodStr]
        
        if event == KEY_LOSS_ARGS_INPUT:
            LossArgs = values[event]

        if event == KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION:
            WithDocumentation = values[KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION]

        if event == KEY_BUTTON_EXPORT_WORKER:
            if DistributedSystemType == "none":
                DistributedSystemToken = "none"
            # Update here when adding new fields to the worker 
            worker_parameters_conditions = bool(LayersSizesList) and bool(ModelTypeStr) and (int(ModelType) >= 0) and\
                                           bool(OptimizationType) and bool(LossMethodStr) and bool(LossMethod) and\
                                           bool(LearningRate) and bool(LayersFunctionsList) and bool(LayersSizesList) and bool(Epochs) and bool(InfraType) and\
                                           bool(DistributedSystemType) and bool(DistributedSystemToken)
            FilePath = Path(FileDirExport) / Path(FileNameExport)
            filepath_condition = FilePath.parent.is_dir() and bool(FileNameExport) and FileNameExport.endswith(".json")
            first_element_condition = bool(LayerTypesList[0] != "3")
            if worker_parameters_conditions and filepath_condition and first_element_condition:
                # Update here when adding new fields to the worker 
                newWorker = Worker("new",LayersSizesList, ModelTypeStr, ModelType, ModelArgsStr, OptimizationType, OptimizationArgs , LossMethodStr, LossArgs, LossMethod,
                                    LearningRate, Epochs, LayersFunctionsList, LayerTypesList, InfraType, DistributedSystemType, DistributedSystemArgs, DistributedSystemToken)
                newWorker.save_as_json(FilePath.as_posix(), WithDocumentation)
                sg.popup_auto_close("Successfully Created", keep_on_top=True)
                break
            elif not worker_parameters_conditions:
                sg.popup_auto_close("Missing Parameters!", keep_on_top=True)
            elif not filepath_condition:
                sg.popup_auto_close("Issue with json path!", keep_on_top=True)
            elif not first_element_condition:
                sg.popup_auto_close("Perceptron can't be the first layer, please change it", keep_on_top=True)

        if event == KEY_JSON_LOAD_FILE_BROWSE_EVENT:
            FilePathLoad = values[KEY_JSON_LOAD_FILE_BROWSE_EVENT]
            # print(f"{FilePathLoad}")
        
        if event == KEY_JSON_LOAD_FILE_BUTTON_EVENT:
            # Update here when adding new fields to the worker
            load_conditions = bool(FilePathLoad) and FilePathLoad.endswith(".json")
            if load_conditions:
                # loading json
                loaded_worker_dict = {}
                with open(FilePathLoad) as jsonFile:
                    loaded_worker_dict = json.load(jsonFile)
                (LayersSizesList, ModelTypeStr, ModelType, ModelArgsStr, OptimizationType, OptimizationArgs ,LossMethodStr, LossMethod, LearningRate, Epochs,
                LayersFunctionsList, LayerTypesList, InfraType, DistributedSystemType, DistributedSystemArgs ,DistributedSystemToken) = Worker.load_from_dict(loaded_worker_dict, get_params=True)
                ui_update_all_values(WorkerWindow)

            else:
                sg.popup_auto_close("Issue with selected json file", keep_on_top=True)

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
        
    WorkerWindow.close()


def LayerMethodSelection():
    global global_layer_method_selection_code

    layout = [[sg.Text("Activation",expand_x=True), sg.Text('Pooling', expand_x=True), sg.Text('Scaler', expand_x=True),],
                [sg.Listbox(list(ActivationFunctionsMap.keys()), size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_ACTIVATION),
                 sg.Listbox(list(PoolingMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_POOLING), 
                 sg.Listbox(list(ScalingMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_SCALER)],
                 [ sg.Text("Bounding",expand_x=True), sg.Text('Flatten', expand_x=True), sg.Text('Probabilistic', expand_x=True)],
                 [
                 sg.Listbox(list(BoundingMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_BOUNDING),
                 sg.Listbox(list(FlattenMethodMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_FLATTEN),
                 sg.Listbox(list(ProbabilisticActivationFunctionMap.keys()),size=(20,15), enable_events=True, key=KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_PROBABILISTIC)
                 ],
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

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_BOUNDING:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_BOUNDING][0]
            global_layer_method_selection_code = BoundingMethodMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_FLATTEN:
            layer_method_selection = values[KEY_LAYER_METHOD_SELECTION_DIALOG_LISTBOX_FLATTEN][0]
            global_layer_method_selection_code = FlattenMethodMap[layer_method_selection]
            layer_selection_win[KEY_LAYER_METHOD_SELECTION_TEXT].update(f'Selected {layer_method_selection} code: {global_layer_method_selection_code}')

        if event == KEY_LAYER_METHOD_SELECTION_BUTTON:
            break
        

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
    
    layer_selection_win.close()
