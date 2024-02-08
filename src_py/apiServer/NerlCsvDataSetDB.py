

class CsvDataSet():
    def __init__(self, csv_path, csv_temp_path, batch_size, num_of_features, num_of_labels):
        self.csv_path = csv_path
        self.csv_temp_path = csv_temp_path
        self.batch_size = batch_size
        self.num_of_features = num_of_features
        self.num_of_labels = num_of_labels

    def get_csv_path(self):
        return self.csv_path
    
    def get_csv_temp_path(self):
        return self.csv_temp_path

    def get_batch_size(self):
        return self.batch_size
    
    def get_num_of_features(self):
        return self.num_of_features

    def get_num_of_labels(self):
        return self.num_of_labels

    def generate_source_pieceDS(self, source_name : str, batch_size, phase : str, starting_offset = 0, num_of_batches = 0):
        return SourcePieceDS(source_name, batch_size, phase, starting_offset, num_of_batches)
        
    

class SourcePieceDS():
    def __init__(self, source_name : str, batch_size, phase : str, starting_offset = 0, num_of_batches = 0):
        self.source_name = source_name
        self.batch_size = batch_size
        self.phase = phase
        self.starting_offset = starting_offset
        self.num_of_batches = num_of_batches
        #self.workers
        self.pointer_to_CsvDataSet = None # which csvDataSet

    def get_source_name(self):
        return self.source_name
    
    def get_batch_size(self):
        return self.batch_size

    def get_phase(self):
        return self.phase

    def get_starting_offset(self):
        return self.starting_offset 
    
