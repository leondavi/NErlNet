
WinExperimentPhasesNamesList = []
ExperimentFlowSourcePieceIdDict = {}
ExperimentFlowSourcePieceIdDict[0] = []

# CSV Settings Event Keys
WIN_EXPFLOW_KEY_CSV_FILE_PATH_CHOSEN_FILE = "-WIN-EXPFLOW-KEY-CSV-FILE-PATH-CHOSEN-FILE-"

# JSON File Settings Event Keys
WIN_EXPFLOW_KEY_JSON_LOAD_FILE_BROWSE_EVENT = "-WIN-EXPFLOW-KEY-JSON-LOAD-FILE-BROWSE-EVENT-"
WIN_EXPFLOW_KEY_JSON_FILE_CHOSEN_DIR = "-WIN-EXPFLOW-KEY-JSON-FILE-CHOSEN-DIR-"
WIN_EXPFLOW_KEY_JSON_FILE_NAME = "-WIN-EXPFLOW-KEY-JSON-FILE-NAME-"
WIN_EXPFLOW_KEY_BUTTON_EXPORT_WORKER = "-WIN-EXPFLOW-KEY-BUTTON-EXPORT-WORKER-"
WIN_EXPFLOW_KEY_JSON_LOAD_FILE_BUTTON_EVENT = "-WIN-EXPFLOW-KEY-JSON-LOAD-FILE-BUTTON-EVENT-"
WIN_EXPFLOW_KEY_CHECKBOX_WORKER_WITH_DOCUMENTATION = "-WIN-EXPFLOW-KEY-CHECKBOX-WORKER-WITH-DOCUMENTATION-"
WIN_EXPFLOW_KEY_NUM_OF_LABELS = "-WIN-EXPFLOW-KEY-NUM-OF-LABELS-"
WIN_EXPFLOW_KEY_NUM_OF_FEATURES = "-WIN-EXPFLOW-KEY-NUM-OF-FEATURES-"
WIN_EXPFLOW_KEY_FIRST_ROW_HEADERS = "-WIN-EXPFLOW-KEY-FIRST-ROW-HEADERS-"
WIN_EXPFLOW_KEY_HEADERS_NAMES = "-WIN-EXPFLOW-KEY-HEADERS-NAMES-"

# Add Phase Event Keys
WIN_EXPFLOW_KEY_PHASE_NAME = "-WIN-EXPFLOW-KEY-PHASE-NAME-"
WIN_EXPFLOW_KEY_BUTTON_ADD_SOURCE_PIECE = "-WIN-EXPFLOW-KEY-BUTTON-ADD-SOURCE-PIECE-"
WIN_EXPFLOW_KEY_BUTTON_ADD_PHASE = "-WIN-EXPFLOW-KEY-BUTTON-ADD-PHASE-"
WIN_EXPFLOW_KEY_BUTTON_SELECT_PHASE = "-WIN-EXPFLOW-KEY-BUTTON-SELECT-PHASE-"
WIN_EXPFLOW_KEY_BUTTON_REMOVE_PHASE = "-WIN-EXPFLOW-KEY-BUTTON-REMOVE-PHASE-"
WIN_EXPFLOW_COMBO_TYPE_LIST = "-WIN-EXPFLOW-COMBO-TYPE-LIST-"
# Add Source Piece Event Keys
WIN_EXPFLOW_KEY_SOUCRE_PIECE_NAME = "-WIN-EXPFLOW-KEY-SOURCE-PIECE-NAME-"
WIN_EXPFLOW_KEY_SOUCRE_PIECE_STARTING_SAMPLE = "-WIN-EXPFLOW-KEY-SOURCE-PIECE-STARTING-SAMPLE-"
WIN_EXPFLOW_KEY_SOUCRE_PIECE_NUM_OF_BATCHES = "-WIN-EXPFLOW-KEY-SOURCE-PIECE-NUM-OF-BATCHES-"
WIN_EXPFLOW_KEY_SOUCRE_PIECE_WORKERS = "-WIN-EXPFLOW-KEY-SOURCE-PIECE-WORKERS-"


# Phases List Box Event Keys
WIN_EXPFLOW_KEY_PHASES_LIST_BOX = "-WIN-EXPFLOW-KEY-PHASES-LIST-BOX-"

# Phase Source Pieces List Box Event Keys
WIN_EXPFLOW_KEY_PHASE_SOURCE_PIECES_LIST_BOX = "-WIN-EXPFLOW-KEY-PHASE-SOURCE-PIECES-LIST-BOX-"

class ExperimentFlowFields:
    def __init__(self):
        self.batch_size = 0
        self.csv_path = ""
        self.num_of_labels = ""
        self.num_of_features = ""
        self.first_row_headers = None
        self.headers_names = None
        self.phases_list = []

class ExperimentPhaseFields:
    PHASE_NAME_STR = "name"
    PHASE_STARTING_SAMPLE_STR = "starting_sample"
    PHASE_NUM_OF_BATCHES_STR = "num_of_batches"
    PHASE_WORKERS_STR = "workers"
    PHASE_SOURCES_STR = "sources"

    def __init__(self, name, phase, starting_sample, num_of_batches, workers_names, sources_names):
        self.name = name
        self.phase = phase
        self.starting_sample = starting_sample
        self.num_of_batches = num_of_batches
        self.workers = workers_names
        self.sources = sources_names