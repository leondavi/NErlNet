

class ExperimentFlowFields:
    def __init__(self):
        self.batch_size = 0
        self.csv_path = ""
        self.num_of_labels = ""
        self.num_of_features = ""
        self.first_row_labels = None
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