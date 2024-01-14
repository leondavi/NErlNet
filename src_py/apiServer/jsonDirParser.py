
import os
import json

from definitions import *

ARCH_IDX = 0
CONN_MAP_IDX = 1
EXPERIMENT_IDX = 2

PREFIX_DC = 'dc_'
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

        return self.json
    
    def __lt__(self, other):
        return self.filename < other.filename

class JsonDirParser():

    def __init__(self, jsonDirPath: str = NERLCONFIG_JSONS_DIR):
        self.jsonDirPathStr = read_nerlconfig(jsonDirPath)

        self.dc_list = []
        self.conn_map_list = []
        self.experiments_list = []

        self.user_selection_tuple = None

        self.extract_lists(self.jsonDirPathStr)
 
    def get_json_dir_path(self):
        return self.jsonDirPathStr
    
    def extract_lists(self, jsonDirPath):
        for dirpath, dirnames, filenames in os.walk(jsonDirPath):
            for filename in filenames:
                if filename.startswith(PREFIX_DC) and filename.endswith('json'): 
                    self.dc_list.append(NerlFile(filename, dirpath))

                if filename.startswith(PREFIX_CONNECTION_MAP) and filename.endswith('json'): 
                    self.conn_map_list.append(NerlFile(filename, dirpath))
                
                if filename.startswith(PREFIX_EXPERIMENT_FLOW) and filename.endswith('json'): 
                    self.experiments_list.append(NerlFile(filename, dirpath))
                    
        self.dc_list.sort()
        self.conn_map_list.sort()
        self.experiments_list.sort()

    def print_lists(self):

        print("\nDistributed Configuration Files\n--------------------\n")
        for idx, elem in enumerate(self.dc_list):
            print(f'{idx}.\t{elem.filename}')
        
        print("\nConnection Map Files\n--------------------\n")
        for idx, elem in enumerate(self.conn_map_list):
            print(f'{idx}.\t{elem.filename}')

        print("\nExperiments Flow Files\n--------------------\n")
        for idx, elem in enumerate(self.experiments_list):
            print(f'{idx}.\t{elem.filename}')


    def set_dc_connmap_experiment(self, dc_num : int, connection_map_num : int, experiment_num : int):
        self.user_selection_tuple = (dc_num, connection_map_num, experiment_num)

    def get_user_selection_files(self):
        if self.user_selection_tuple:
            try:
                selectedDC = self.dc_list[self.user_selection_tuple[ARCH_IDX]].get_full_path()
                selectedConn = self.conn_map_list[self.user_selection_tuple[CONN_MAP_IDX]].get_full_path()
                selectedExp  = self.experiments_list[self.user_selection_tuple[EXPERIMENT_IDX]].get_full_path()
                return selectedDC, selectedConn, selectedExp
            except:
                print("bad select vector!")
                return None, None, None

        return None, None, None

    def get_user_selection_jsons(self):
        if self.user_selection_tuple:
            try:
                selectedArch = self.dc_list[self.user_selection_tuple[ARCH_IDX]].get_json()
                selectedConn = self.conn_map_list[self.user_selection_tuple[CONN_MAP_IDX]].get_json()
                selectedExp  = self.experiments_list[self.user_selection_tuple[EXPERIMENT_IDX]].get_json()

                return selectedArch, selectedConn, selectedExp

            except:
                return None, None, None
        return None, None, None

    def json_from_path(self, json_path : str):
        return json.load(open(json_path))