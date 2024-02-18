import os
import json
from pathlib import Path
from collections import OrderedDict
from logger import *
# nerlconfig files

NERLNET_PATH = "/usr/local/lib/nerlnet-lib/NErlNet"
NERLNET_SRC_PY_PATH = f"{NERLNET_PATH}/src_py"
NERLCONFIG_INPUT_DATA_DIR = "/usr/local/lib/nerlnet-lib/NErlNet/config/inputDataDir.nerlconfig"
NERLCONFIG_JSONS_DIR = '/usr/local/lib/nerlnet-lib/NErlNet/config/jsonsDir.nerlconfig'

NERLCONFIG_SUFFIX = ".nerlconfig"
INPUT_DATA_DIR_CONF = "inputDataDir"
JSONS_DIR = "jsonsDir"
# Should be exatly as ?LOCAL_DC_FILE_NAME ?LOCAL_COMM_FILE_NAME
DC_FILE_ARCH_REMOTE_NAME = '/tmp/nerlnet/jsons/dc.json' #TODO get back to this after taking care to multipart
JSON_FILE_COMM_REMOTE_NAME = '/tmp/nerlnet/jsons/conn.json'

JSON_INIT_HANDLER_ERL_PORT = 8484 #TODO fix main server bypassing

NERLNET_TEMP_DIR = '/tmp/nerlnet'
NERLNET_TEMP_DATA_DIR = f'{NERLNET_TEMP_DIR}/temp_data'
EXPERIMENT_RESULTS_PATH = f'{NERLNET_TEMP_DIR}/results'

PHASE_TRAINING = 1
PHASE_PREDICTION = 2
PHASE_STATS = 3 # TODO maybe redundant


PHASE_TRAINING_STR = "training"
PHASE_PREDICTION_STR = "prediction"

def read_nerlconfig(nerlconfig_file_path : str):
    if not nerlconfig_file_path.endswith(NERLCONFIG_SUFFIX):
        raise "wrong filename suffix"
    if not os.path.isfile(nerlconfig_file_path):
        raise "nerlconfig does not exist!"
    with open(nerlconfig_file_path) as file:
        if (INPUT_DATA_DIR_CONF in nerlconfig_file_path) or (JSONS_DIR in nerlconfig_file_path):
            lines = file.readlines()
            if lines:
                first_line = lines[0].rstrip()
                if os.path.exists(first_line):
                    return first_line
                else:
                    if (INPUT_DATA_DIR_CONF in nerlconfig_file_path):
                        LOG_ERROR("data directory does not exist in given path , consider running ./NerlnetGetData.sh")
                    LOG_ERROR(f"bad nerlconfig directory is given: {first_line} at {nerlconfig_file_path}")
                    raise "bad nerlconfig directory is given"
    return None

def is_port_free(port: int) -> bool:
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) != 0
    

def search_file(filename : str , rootdir : str) -> str:
    for root, _, files in os.walk(rootdir):
        if filename in files:
            return os.path.join(root, filename)
    return None

def export_dict_json(filepath : str , dict : OrderedDict):
    Path(filepath).parent.mkdir(parents=True, exist_ok=True)
    json_obj = json.dumps(dict, indent=4)

    # Writing to sample.json
    with open(filepath, "w") as outfile:
        outfile.write(json_obj)
        
def import_dict_json(filepath : str):
    with open(filepath, "r") as infile:
        return json.load(infile , object_pairs_hook=OrderedDict)