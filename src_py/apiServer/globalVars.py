################################################
# Nerlnet - 2023 GPL-3.0 license
# Authors: Haran Cohen, David Leon, Dor Yerchi #
################################################
import socket
from IPython import get_ipython
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

TRUE_LABLE_IND = 0
PRED_LABLE_IND = 1

# Check if we are running on Jupyter Notebook, to disable logging prompts:
IPYTHON_PLATFORM = str(type(get_ipython()))
