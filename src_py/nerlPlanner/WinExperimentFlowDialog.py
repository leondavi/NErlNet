import PySimpleGUI as sg

from WinExperimentFlowDialogDefinitions import *

EXPERIMENT_FLOW_WINDOW_TITLE = "Generate Experiment Flow Json File (In Development)"

def WinExperimentFlowDialog():
    ExperimentFlowDatasetLayout = [[sg.Text("csv file path: "),sg.In(enable_events=True ,key=WIN_EXPFLOW_KEY_CSV_FILE_PATH_CHOSEN_FILE, expand_x=True), sg.FileBrowse(file_types=(("csv file", "*.csv"),))],
                                   [sg.Text("Number of labels"),sg.InputText(key=WIN_EXPFLOW_KEY_NUM_OF_LABELS, enable_events=True, expand_x=True)],
                                   [sg.Text("Number of features"),sg.InputText(key=WIN_EXPFLOW_KEY_NUM_OF_FEATURES, enable_events=True, expand_x=True)],
                                   [sg.Text("First row headers"),sg.Checkbox("Yes", key=WIN_EXPFLOW_KEY_FIRST_ROW_HEADERS, enable_events=True)],
                                   [sg.Text("Headers names"),sg.InputText(key=WIN_EXPFLOW_KEY_HEADERS_NAMES, enable_events=True, expand_x=True)]]
    ExperimentFlowDatasetFrame = sg.Frame("Dataset Settings",layout=ExperimentFlowDatasetLayout, expand_x=True)
    
    ExperimentFlowAddPhaseLayout = [[sg.Text("Phase name"),sg.InputText(key=WIN_EXPFLOW_KEY_PHASE_NAME, enable_events=True, expand_x=True)],
                                    [sg.Text("Starting sample"),sg.InputText(key=WIN_EXPFLOW_KEY_PHASE_STARTING_SAMPLE, enable_events=True, expand_x=True)],
                                    [sg.Text("Number of batches"),sg.InputText(key=WIN_EXPFLOW_KEY_PHASE_NUM_OF_BATCHES, enable_events=True, expand_x=True)],
                                    [sg.Text("Workers"),sg.InputText(key=WIN_EXPFLOW_KEY_PHASE_WORKERS, enable_events=True, expand_x=True)],
                                    [sg.Text("Sources"),sg.InputText(key=WIN_EXPFLOW_KEY_PHASE_SOURCES, enable_events=True, expand_x=True)],
                                    [sg.Button("Add", key=WIN_EXPFLOW_KEY_BUTTON_ADD_PHASE, enable_events=True, expand_x=True),
                                     sg.Button("Select", key=WIN_EXPFLOW_KEY_BUTTON_SELECT_PHASE, enable_events=True, expand_x=True),
                                     sg.Button("Remove", key=WIN_EXPFLOW_KEY_BUTTON_REMOVE_PHASE, enable_events=True, expand_x=True)]
                                   ]
    ExperimentFlowAddPhaseFrame = sg.Frame("Add Phase",layout=ExperimentFlowAddPhaseLayout)

    ExpermentFlowPhasesListLayout = [[sg.Listbox(WinExperimentPhasesNamesList, size=(30,10), expand_x=True, key=WIN_EXPFLOW_KEY_PHASES_LIST_BOX, enable_events=True)]]
    ExperimentFlowPhasesListFrame = sg.Frame("Phases",layout=ExpermentFlowPhasesListLayout)

    ExperimentFlowPhaseFrame = sg.Frame("Phase",layout=[[ExperimentFlowAddPhaseFrame, ExperimentFlowPhasesListFrame]], expand_x=True)


    ExperimentFlowFileLayout = [[sg.Text("Load json"),sg.In(enable_events=True ,key=WIN_EXPFLOW_KEY_JSON_LOAD_FILE_BROWSE_EVENT, expand_x=True), sg.FileBrowse(file_types=(("Json File", "*.json"),)), sg.Button("Load", key=WIN_EXPFLOW_KEY_JSON_LOAD_FILE_BUTTON_EVENT, enable_events=True)],
                                [sg.Text("Select json file output directory"),sg.In(enable_events=True ,key=WIN_EXPFLOW_KEY_JSON_FILE_CHOSEN_DIR, expand_x=True), sg.FolderBrowse()],
                                [sg.Text("*.json file name"),sg.InputText(key=WIN_EXPFLOW_KEY_JSON_FILE_NAME, enable_events=True),sg.Button("Export",key=WIN_EXPFLOW_KEY_BUTTON_EXPORT_WORKER), sg.Checkbox('with documentation', default=True, key=WIN_EXPFLOW_KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION, enable_events=True)]]

    ExperimentFlowFileFrame = sg.Frame("File",layout=ExperimentFlowFileLayout, expand_x=True)

    ExperimentFlowWindow  = sg.Window(title=EXPERIMENT_FLOW_WINDOW_TITLE, layout=[[ExperimentFlowDatasetFrame],[ExperimentFlowPhaseFrame],[ExperimentFlowFileFrame]],modal=True, keep_on_top=True)                                                  
    while True:
        event, values = ExperimentFlowWindow.read()

        if event == "Exit" or event == sg.WIN_CLOSED:
            break
    ExperimentFlowWindow.close()