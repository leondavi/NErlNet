################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import socket
from IPython import get_ipython
from experiment import *
from workerResult import *
from time import sleep
from logger import *
from NerlComDB import *

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

# A single experiment that API server is focused on get/send data
experiment_focused_on = None # TODO - get rid of this as a global
components = None # TODO - get rid of this as a global 
# nerlcom_db = NerlComDB() # TODO - get rid of this as a global

# for each server command, wait for appropriate number of acks from each entity to make sure job is finished
# this may change according to command and which entities do the work
pendingAcks = 0

# Get the components of the current system:
ARCHITECTURE_INDEX = 4
GRAPH_INDEX = 5
# Entity modes / commands
TRAINING_STR = "Training"
PREDICTION_STR = "Prediction"
BATHCHES_PER_SOURCE_STR = "Batches per source"

PHASE_TRAINING_STR = "training"
PHASE_PREDICTION_STR = "prediction"

# Dict with {worker : csv}:
workerCsv = {}
sourceCSVIndex = 0
# predictions const
MATRIX_DISP_SCALING = 5
TRUE_LABLE_IND = 0
PRED_LABLE_IND = 1

# splitMode for data in sources (1 = split file by sources, 2 = send whole to all)
CSVsplit = None

# Check if we are running on Jupyter Notebook, to disable logging prompts:
ipythonPlatform = str(type(get_ipython()))

if 'zmqshell' in ipythonPlatform: # Check if runnnig on Jupyter Notebook.
    jupyterFlag =  True 
else: 
    jupyterFlag = False

# Global variables


# Global functions

def set_receiver_wait_for_ack():
    global pendingAcks
    pendingAcks += 1
    return pendingAcks

def waitForAck():
    global pendingAcks

    while pendingAcks > 0:
        sleep(0.005)

def ack_debug_print(debug = False):
    global pendingAcks
    if debug:
        LOG_DEBUG(f"debug pending acks: {pendingAcks}")