
import os
import json

JSON_DIRECTORY = '/usr/local/lib/nerlnet-lib/NErlNet/jsonDir'

PREFIX_ARCH = 'arch_'
PREFIX_CONNECTION_MAP = 'conn_'
PREFIX_EXPERIMENT_FLOW = 'exp_'

class NerlFile():
    def __init__(self, filename : str, filepath: str):
        self.filename = filename
        self.filepath = filepath

    def get_filename(self) -> str:
        return self.filename

    def get_filepath(self) -> str:
        return self.filepath

    def get_full_path(self) -> str:
        return f'{self.filepath}/{self.filename}'
    
    def __format__(self,spec):
        return f'{self.filepath}/{self.filename}'

    def get_json(self):
        # Opening JSON file
        f = open(self.get_full_path())
        self.json = json.load(f)

class JsonDirParser():

    def __init__(self, jsonDirPath: str = JSON_DIRECTORY):
        self.jsonDirPathStr = ''
        with open(jsonDirPath) as f:
            lines = f.readlines()
            if lines:
                self.jsonDirPathStr = lines[0]

        self.arch_list = []
        self.conn_map_list = []
        self.experiments_list = []

        self.user_selection_tuple = None

        self.extract_lists()
 
    
    def extract_lists(self):
        for (dirpath, dirnames, filenames) in os.walk(self.jsonDirPathStr):
            for filename in filenames:
                if filename.startswith(PREFIX_ARCH) and filename.endswith('json'): 
                    self.arch_list.append(NerlFile(filename, dirpath))
                if filename.startswith(PREFIX_CONNECTION_MAP) and filename.endswith('json'): 
                    self.conn_map_list.append(NerlFile(filename, dirpath))
                if filename.startswith(PREFIX_EXPERIMENT_FLOW) and filename.endswith('json'): 
                    self.experiments_list.append(NerlFile(filename, dirpath))


    def print_lists(self):
        print("\nArchitechure Files\n--------------------\n")
        for idx, elem in enumerate(self.arch_list):
            print(f'{idx}. {elem}')
        
        print("\nConnection Map Files\n--------------------\n")
        for idx, elem in enumerate(self.conn_map_list):
            print(f'{idx}. {elem}')

        print("\nExperiments Flow Files\n--------------------\n")
        for idx, elem in enumerate(self.experiments_list):
            print(f'{idx}. {elem}')

    def select_arch_connmap_experiment(self, arch : int, connection_map : int, experiment : int):
        self.user_selection_tuple = (arch, connection_map, experiment)

    def get_user_selection_files(self):
        if self.user_selection_tuple:
            ARCH_IDX = 0
            CONN_MAP_IDX = 1
            EXPERIMENT_IDX = 2
            return self.arch_list[self.user_selection_tuple[ARCH_IDX]].get_full_path(), self.conn_map_list[self.user_selection_tuple[CONN_MAP_IDX]].get_full_path(), self.experiments_list[self.user_selection_tuple[EXPERIMENT_IDX]].get_full_path()
        return None, None, None