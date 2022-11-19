###########################################################
##### Author: Dor Yarchi
# Copyright: © 2022
# Date: 27/07/2022
###########################################################
import multiprocessing 
import socket
import os
import json
from IPython import get_ipython
from experiment import *
from csvResult import *
from workerResult import *

localHost = socket.gethostname()
localIp = socket.gethostbyname(localHost)

pendingAcks = 0

multiProcQueue = multiprocessing.Queue() # Create an instance of the queue

# Get the components of the current system:
ARCHITECTURE_INDEX = 4
GRAPH_INDEX = 5


username = os.getlogin()
#jsonPathLocation = '/home/{}/workspace/NErlNet/jsonPath'.format(username) # Use this if NerlnetInstall.sh does not work
jsonPathLocation = '/usr/local/lib/nerlnet-lib/NErlNet/jsonPath'
jsonPath = open(jsonPathLocation)
content = jsonPath.readlines()
# Get the components of the current system:
componentsPath = content[ARCHITECTURE_INDEX][:-1]
# Get the flow of the current experiment:

# Dict with {worker : csv}:
workerCsv = {}

# Check if we are running on Jupyter Notebook, to disable logging prompts:
ipythonPlatform = str(type(get_ipython()))

if 'zmqshell' in ipythonPlatform: # Check if runnnig on Jupyter Notebook.
    jupyterFlag =  True 

else: 
    jupyterFlag = False

# Global variables
components = None # will be initialized in ApiServer
# Prepare to get results from the receiver:
experiment_flow_global = Experiment()

if __name__ == "__main__":
    
    print(content[0])
    print(content[1])

# Addresses for future development:
'''
trainingListReq = [('http://127.0.0.1:8080/updateCSV', "s1,w1,RunOrWalkTrain_splitted"),
                ('http://127.0.0.1:8080/clientsTraining', "")]

CastingListReq = [('http://127.0.0.1:8080/startCasting', "s1")]
'''
