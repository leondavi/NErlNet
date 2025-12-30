import os
import json
from pathlib import Path
from collections import OrderedDict
from logger import *
from pathlib import Path
import pickle
import pandas as pd

# nerlconfig files

NERLNET_PATH = "/usr/local/lib/nerlnet-lib/NErlNet"
NERLNET_SRC_PY_PATH = f"{NERLNET_PATH}/src_py"
NERLCONFIG_JSONS_DIR = '/usr/local/lib/nerlnet-lib/NErlNet/config/jsonsDir.nerlconfig'
DEFAULT_NERLNET_TMP_DATA_DIR = '/tmp/nerlnet/data/NerlnetData-master/nerlnet'
HF_DATA_REPO_PATHS_JSON = f'{NERLNET_PATH}/src_py/apiServer/hf_repo_ids.json'

NERLCONFIG_SUFFIX = ".nerlconfig"
INPUT_DATA_DIR_CONF = "inputDataDir"
JSONS_DIR = "jsonsDir"
# Should be exatly as ?LOCAL_DC_FILE_NAME ?LOCAL_COMM_FILE_NAME
DC_FILE_ARCH_REMOTE_NAME = '/tmp/nerlnet/jsons/dc.json' #TODO get back to this after taking care to multipart
JSON_FILE_COMM_REMOTE_NAME = '/tmp/nerlnet/jsons/conn.json'
TORCH_REMOTE_MODEL_ROOT = '/tmp/nerlnet/torch/models/pt'

JSON_INIT_HANDLER_ERL_PORT = 8484 #TODO fix main server bypassing

NERLNET_TEMP_DIR = '/tmp/nerlnet'
NERLNET_TEMP_DATA_DIR = f'{NERLNET_TEMP_DIR}/temp_data'
EXPERIMENT_RESULTS_PATH = f'{NERLNET_TEMP_DIR}/results'

PHASE_TRAINING = 1
PHASE_PREDICTION = 2
PHASE_STATS = 3 # TODO maybe redundant

SOURCE_POLICY_CASTING = "0" # TODO check import from NerlPlanner
SOURCE_POLICY_ROUND_ROBIN = "1" # TODO check import from NerlPlanner

PHASE_TRAINING_STR = "training"
PHASE_PREDICTION_STR = "prediction"

NERLTENSOR_TYPE_LIST = ['float', 'int16', 'int32', 'double', 'uint8']

def read_nerlconfig(nerlconfig_file_path : str):
    if not nerlconfig_file_path.endswith(NERLCONFIG_SUFFIX):
        raise "wrong filename suffix"
    if not os.path.isfile(nerlconfig_file_path):
        raise "nerlconfig does not exist!"
    with open(nerlconfig_file_path) as file:
        if (JSONS_DIR in nerlconfig_file_path):
            lines = file.readlines()
            if lines:
                first_line = lines[0].rstrip()
                if os.path.exists(first_line):
                    return first_line
                else:
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

def export_dict_pickle(filepath : str , dict : OrderedDict):
    Path(filepath).parent.mkdir(parents=True, exist_ok=True)
    with open(filepath, 'wb') as handle:
        pickle.dump(dict, handle, protocol=pickle.HIGHEST_PROTOCOL)
        
def export_df_csv(filepath : str , df):
    Path(filepath).parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(filepath, index=False)
    
def import_csv_df(filepath : str):
    if not os.path.isfile(filepath):
        LOG_ERROR(f"File does not exist: {filepath}")
        raise FileNotFoundError(f"File does not exist: {filepath}")
    return pd.read_csv(filepath)

def import_dict_pickle(filepath : str):
    if not os.path.isfile(filepath):
        LOG_ERROR(f"File does not exist: {filepath}")
        raise FileNotFoundError(f"File does not exist: {filepath}")
    with open(filepath, 'rb') as handle:
        return pickle.load(handle)

def export_dict_json(filepath : str , dict : OrderedDict):
    Path(filepath).parent.mkdir(parents=True, exist_ok=True)
    json_obj = json.dumps(dict, indent=4)

    # Writing to sample.json
    with open(filepath, "w") as outfile:
        outfile.write(json_obj)

def is_file_exists(filepath : str) -> bool:
    return os.path.isfile(filepath)
        
def import_dict_json(filepath : str):
    if not os.path.isfile(filepath):
        LOG_ERROR(f"File does not exist: {filepath}")
        raise FileNotFoundError(f"File does not exist: {filepath}")
    with open(filepath, "r") as infile:
        return json.load(infile , object_pairs_hook=OrderedDict)

def average_list(list : list) -> float:
    return sum(list) / len(list)

def pretty_dict(d):
    pretty_dict_str = ''
    # Print the dict nicely
    for k, v in d.items():
        if isinstance(v, list):
            v = ', '.join(map(str, v))
        elif isinstance(v, dict):
            v = pretty_dict(v)
        pretty_dict_str += f'{k}: {str(v)}\n'
    return pretty_dict_str

def build_torch_remote_model_path(model_sha : str, artifact_name : str) -> str:
    """Return the canonical remote path for a Torch artifact referenced by model_sha."""
    safe_sha = ''.join(ch for ch in model_sha if ch.isalnum())
    safe_name = os.path.basename(artifact_name)
    return os.path.join(TORCH_REMOTE_MODEL_ROOT, safe_sha, safe_name)