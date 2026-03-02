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
        raise ValueError(f"wrong filename suffix for nerlconfig: {nerlconfig_file_path}")
    if not os.path.isfile(nerlconfig_file_path):
        raise FileNotFoundError(f"nerlconfig does not exist: {nerlconfig_file_path}")

    with open(nerlconfig_file_path, encoding="utf-8") as file:
        if (JSONS_DIR in nerlconfig_file_path):
            lines = [line.strip() for line in file.readlines() if line.strip()]
            if not lines:
                raise ValueError(f"jsons nerlconfig is empty: {nerlconfig_file_path}")

            configured_path = os.path.expanduser(lines[0])
            if not os.path.isabs(configured_path):
                configured_path = os.path.abspath(
                    os.path.join(os.path.dirname(nerlconfig_file_path), configured_path)
                )

            if os.path.isfile(configured_path):
                LOG_ERROR(
                    f"bad nerlconfig directory is given: {configured_path} at {nerlconfig_file_path} "
                    f"(path points to a file)"
                )
                raise NotADirectoryError(
                    f"Configured JSON directory points to a file: {configured_path}"
                )

            if not os.path.isdir(configured_path):
                LOG_WARNING(
                    f"configured json directory does not exist: {configured_path}; creating it"
                )
                try:
                    os.makedirs(configured_path, exist_ok=True)
                except OSError as exc:
                    LOG_ERROR(
                        f"bad nerlconfig directory is given: {configured_path} at {nerlconfig_file_path}"
                    )
                    raise FileNotFoundError(
                        f"Configured JSON directory cannot be created: {configured_path}"
                    ) from exc
            return configured_path
    return None

def is_port_free(port: int, host: str = "localhost") -> bool:
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        try:
            s.bind((host, port))
            return True
        except OSError:
            return False
    

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


def _format_us_to_human(us_val):
    """Convert microseconds to a human-readable string."""
    if us_val == 0:
        return "0"
    if us_val < 1000:
        return f"{us_val} us"
    if us_val < 1_000_000:
        return f"{us_val / 1000:.2f} ms"
    return f"{us_val / 1_000_000:.3f} s"


def _format_bytes_to_human(byte_val):
    """Convert bytes to a human-readable string."""
    if byte_val == 0:
        return "0"
    if byte_val < 1024:
        return f"{byte_val} B"
    if byte_val < 1024 ** 2:
        return f"{byte_val / 1024:.2f} KB"
    if byte_val < 1024 ** 3:
        return f"{byte_val / (1024 ** 2):.2f} MB"
    return f"{byte_val / (1024 ** 3):.2f} GB"


def _format_cpu_cores(train_cores, predict_cores):
    """Format per-core CPU utilization for train and predict side by side."""
    all_cores = sorted(set(list(train_cores.keys()) + list(predict_cores.keys())),
                       key=lambda x: int(x) if isinstance(x, (int, str)) and str(x).isdigit() else x)
    parts = []
    for core in all_cores:
        t = train_cores.get(core, 0)
        p = predict_cores.get(core, 0)
        parts.append(f"core {core}: {t:.1f}% / {p:.1f}%")
    return ", ".join(parts) if parts else "N/A"


def format_performance_stats(perf_train, perf_predict,
                             workers_comm_train=None, workers_comm_predict=None,
                             worker_to_client=None):
    """Format combined train/predict performance stats into a readable table.

    Args:
        perf_train: dict from get_performance_stats_clients() for training phase
        perf_predict: dict from get_performance_stats_clients() for prediction phase
        workers_comm_train: optional dict from get_communication_stats_workers() for training
        workers_comm_predict: optional dict from get_communication_stats_workers() for prediction
        worker_to_client: optional dict mapping worker_name -> client_name

    Returns:
        Formatted string with unified performance stats.
    """
    lines = []
    all_clients = list(dict.fromkeys(list(perf_train.keys()) + list(perf_predict.keys())))
    w2c = worker_to_client or {}

    for client_name in all_clients:
        t = perf_train.get(client_name, {})
        p = perf_predict.get(client_name, {})

        lines.append(f"Performance stats for {client_name}:")
        lines.append(f"  {'':30s} {'Training':>16s}   {'Prediction':>16s}")
        lines.append(f"  {'─' * 66}")

        # Time
        t_active = t.get('time_train_active', 0)
        t_total = t.get('time_train_total', 0)
        p_active = p.get('time_predict_active', 0)
        p_total = p.get('time_predict_total', 0)

        # Per-worker NIF time aggregation, filtered to this client's workers
        has_worker_data = workers_comm_train or workers_comm_predict
        wc_train_all = workers_comm_train or {}
        wc_predict_all = workers_comm_predict or {}

        if w2c:
            wc_train = {w: s for w, s in wc_train_all.items() if w2c.get(w) == client_name}
            wc_predict = {w: s for w, s in wc_predict_all.items() if w2c.get(w) == client_name}
        else:
            wc_train = wc_train_all
            wc_predict = wc_predict_all

        num_workers_train = len(wc_train)
        num_workers_predict = len(wc_predict)

        t_total_nif = sum(int(ws.get('acc_time_training', 0) or 0) for ws in wc_train.values())
        p_total_nif = sum(int(ws.get('acc_time_prediction', 0) or 0) for ws in wc_predict.values())

        if has_worker_data and (t_total_nif > 0 or p_total_nif > 0):
            lines.append(f"  {'Total NIF compute (all workers)':30s} {_format_us_to_human(t_total_nif):>16s}   {_format_us_to_human(p_total_nif):>16s}")
            lines.append(f"  {'Total wall-clock time':30s} {_format_us_to_human(t_total):>16s}   {_format_us_to_human(p_total):>16s}")
            lines.append(f"  {'Number of workers':30s} {num_workers_train:>16d}   {num_workers_predict:>16d}")

            # Compute utilization: total NIF time across all workers / (num_workers * wall_time)
            t_util = (t_total_nif / (num_workers_train * t_total) * 100) if (t_total > 0 and num_workers_train > 0) else 0
            p_util = (p_total_nif / (num_workers_predict * p_total) * 100) if (p_total > 0 and num_workers_predict > 0) else 0
            lines.append(f"  {'Compute utilization':30s} {t_util:>15.2f}%   {p_util:>15.2f}%")

            # Idle fraction
            t_idle = max(0, 100 - t_util)
            p_idle = max(0, 100 - p_util)
            lines.append(f"  {'Idle fraction':30s} {t_idle:>15.2f}%   {p_idle:>15.2f}%")

            # Per-worker breakdown
            all_worker_names = sorted(set(list(wc_train.keys()) + list(wc_predict.keys())))
            lines.append(f"  {'Per-worker NIF time':30s}")
            for wname in all_worker_names:
                wt = int(wc_train.get(wname, {}).get('acc_time_training', 0) or 0)
                wp = int(wc_predict.get(wname, {}).get('acc_time_prediction', 0) or 0)
                lines.append(f"    {wname:28s} {_format_us_to_human(wt):>16s}   {_format_us_to_human(wp):>16s}")
        else:
            # Single-worker / no worker data display
            lines.append(f"  {'Active compute time':30s} {_format_us_to_human(t_active):>16s}   {_format_us_to_human(p_active):>16s}")
            lines.append(f"  {'Total wall-clock time':30s} {_format_us_to_human(t_total):>16s}   {_format_us_to_human(p_total):>16s}")

            # Utilization ratio
            t_util = (t_active / t_total * 100) if t_total > 0 else 0
            p_util = (p_active / p_total * 100) if p_total > 0 else 0
            lines.append(f"  {'Compute utilization':30s} {t_util:>15.2f}%   {p_util:>15.2f}%")

        # Memory
        t_mem_ema = t.get('memory_train_ema_usage', 0)
        t_mem_peak = t.get('memory_train_peak_usage', 0)
        p_mem_ema = p.get('memory_predict_ema_usage', 0)
        p_mem_peak = p.get('memory_predict_peak_usage', 0)
        lines.append(f"  {'Memory EMA usage':30s} {_format_bytes_to_human(t_mem_ema):>16s}   {_format_bytes_to_human(p_mem_ema):>16s}")
        lines.append(f"  {'Memory peak usage':30s} {_format_bytes_to_human(t_mem_peak):>16s}   {_format_bytes_to_human(p_mem_peak):>16s}")

        # GPU
        t_gpu = t.get('average_gpu_usage_train', 0)
        p_gpu = p.get('average_gpu_memory_usage_predict', 0)
        lines.append(f"  {'Average GPU usage':30s} {t_gpu:>15.1f}%   {p_gpu:>15.1f}%")

        # CPU per core
        num_cores = p.get('num_of_cores', t.get('num_of_cores', 0))
        t_cpu = t.get('cpu_train_util_per_core', {})
        p_cpu = p.get('cpu_predict_util_per_core', {})
        lines.append(f"  {'CPU cores':30s} {num_cores}")
        lines.append(f"  {'CPU utilization (train/pred)':30s} {_format_cpu_cores(t_cpu, p_cpu)}")

    return "\n".join(lines)

def build_torch_remote_model_path(model_sha : str, artifact_name : str) -> str:
    """Return the canonical remote path for a Torch artifact referenced by model_sha."""
    safe_sha = ''.join(ch for ch in model_sha if ch.isalnum())
    safe_name = os.path.basename(artifact_name)
    return os.path.join(TORCH_REMOTE_MODEL_ROOT, safe_sha, safe_name)
