import os

# nerlconfig files
NERLCONFIG_INPUT_DATA_DIR = "/usr/local/lib/nerlnet-lib/NErlNet/config/inputDataDir.nerlconfig"
NERLCONFIG_JSONS_DIR = '/usr/local/lib/nerlnet-lib/NErlNet/config/jsonsDir.nerlconfig'

NERLCONFIG_SUFFIX = ".nerlconfig"
INPUT_DATA_DIR_CONF = "inputDataDir"
JSONS_DIR = "jsonsDir"
JSON_FILE_ARCH_REMOTE_NAME = '/tmp/nerlnet/jsons/arch.json' #TODO get back to this after taking care to multipart
JSON_FILE_COMM_REMOTE_NAME = '/tmp/nerlnet/jsons/conn.json'

JSON_INIT_HANDLER_ERL_PORT = 8484 #TODO fix main server bypassing

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
                    print(f"bad nerlconfig directory is given: {first_line} at {nerlconfig_file_path}")
                    raise "bad nerlconfig directory is given"
    return None